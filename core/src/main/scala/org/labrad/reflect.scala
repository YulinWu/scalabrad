package org.labrad

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import org.labrad.annotations.Setting
import org.labrad.data._
import org.labrad.types._
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.{Context => MacroContext}


case class RequestContext(source: Long, context: Context, id: Long, data: Data)

object Reflect {

  def makeHandler[T: TypeTag]: (Seq[SettingInfo], T => RequestContext => Data) = {
    val t = typeOf[T]
    val allMethods = t.members.toSeq.collect { case m: MethodSymbol => m }
    val overloads = allMethods.groupBy(_.name.decodedName.toString).toSeq

    // build handler for each set of overloaded methods
    val handlers = for {
      (methodName, methods) <- overloads
      annots = methods.flatMap(settingAnnotation)
      if annots.length > 0
    } yield {
      require(annots.length == 1, s"Multiple overloads of '$methodName' have @Setting annotation")
      val (id, name, doc) = annots(0)

      val methodsWithDefaults = for (m <- methods) yield {
        val defaults = allMethods.filter(_.name.decodedName.toString.startsWith(m.name.decodedName.toString + "$default$"))
        (m, defaults)
      }
      val (accepts, returns, binder) = SettingHandler.forMethods(methodsWithDefaults)
      val acceptsInfo = TypeInfo(accepts, accepts.expand.map(_.toString))
      val returnsInfo = TypeInfo(returns, returns.expand.map(_.toString))
      val settingInfo = SettingInfo(id, name, doc.stripMargin, acceptsInfo, returnsInfo)

      (settingInfo, binder)
    }

    val infos = handlers.map { case (info, _) => info }.sortBy(_.id)
    val binders = handlers.map { case (info, binder) => info.id -> binder }.toMap

    // check that setting ids and names are unique
    for ((id, settings) <- infos.groupBy(_.id)) require(settings.length == 1, s"Multiple settings with id $id in type $t")
    for ((name, settings) <- infos.groupBy(_.name)) require(settings.length == 1, s"Multiple settings with name '$name' in type $t")

    val binder = (self: T) => {
      val handlerMap = binders.map { case (id, binder) => id -> binder(self) }
      (req: RequestContext) =>
        handlerMap.get(req.id).map(_(req)).getOrElse(Error(1, s"Setting not found: ${req.id}"))
    }
    (infos, binder)
  }


  // extract information from an optional @Setting annotation on a symbol
  private def settingAnnotation(s: Symbol): Option[(Long, String, String)] =
    for {
      a <- s.annotations.find(_.tree.tpe =:= typeOf[Setting])
      id <- a.param("id").map { case Constant(id: Long) => id }
      name <- a.param("name").map { case Constant(name: String) => name }
      doc <- a.param("doc").map { case Constant(doc: String) => doc }
    } yield (id, name, doc)

  implicit class RichAnnotation(a: Annotation) {
    // extract a single parameter by name
    def param(name: String): Option[Constant] = {
      a.tree.children.tail.collectFirst {
        case AssignOrNamedArg(Ident(TermName(`name`)), Literal(c)) => c
      }
    }
  }
}


/**
 * An object that can handle labrad calls.
 */
trait Handler {
  def call(data: Data)(implicit ec: ExecutionContext): Future[Data]

  val settingId: Long
  val settingName: String
  val accepts: Seq[Pattern]
  val returns: Pattern

  def checkArgs(data: Data): Unit = {
    if (!accepts.exists(_.accepts(data.t))) {
      sys.error(s"data of type ${data.t} does not match any accepted types ${accepts.mkString(",")}")
    }
  }
}

class SyncSettingHandler[B](
  val settingId: Long,
  val settingName: String,
  val accepts: Seq[Pattern],
  val returns: Pattern,
  invoke: Data => B,
  convert: ToData[B]
) extends Handler {
  def call(data: Data)(implicit ec: ExecutionContext): Future[Data] = {
    Future {
      checkArgs(data)
      val result = invoke(data)
      convert(result)
    }
  }
}

class AsyncSettingHandler[B](
  val settingId: Long,
  val settingName: String,
  val accepts: Seq[Pattern],
  val returns: Pattern,
  invoke: Data => Future[B],
  convert: ToData[B]
) extends Handler {
  def call(data: Data)(implicit ec: ExecutionContext): Future[Data] = {
    Future {
      checkArgs(data)
    }.flatMap { _ =>
      invoke(data)
    }.map { result =>
      convert(result)
    }
  }
}


// class ProxyInvocationHandler(
//   cxn: Connection,
//   calls: Map[String, (String, Seq[(String, Writes[_])], Reads[_])],
//   notifys: Map[String, (String, Seq[(String, Writes[_])])]
// ) extends InvocationHandler {
//   def invoke(proxy: Object, method: Method, args: Array[Object]): Object = {
//     for ((rpcMethod, paramDefs, reader) <- calls.get(method.getName)) {
//       val params = Map.newBuilder[String, JsValue]
//       for (((paramName, writer), arg) <- paramDefs zip args) {
//         params += paramName -> writer.writes(arg.asInstanceOf[Nothing])
//       }
//       implicit val ec = args.last.asInstanceOf[ExecutionContext]
//       val f = endpoint.call(src = null, method = rpcMethod, params = Some(Right(params.result)))
//       return f.map { json => reader.reads(json).get }
//     }

//     for ((rpcMethod, paramDefs) <- notifys.get(method.getName)) {
//       val params = Map.newBuilder[String, JsValue]
//       for (((paramName, writer), arg) <- paramDefs zip args) {
//         params += paramName -> writer.writes(arg.asInstanceOf[Nothing])
//       }
//       endpoint.notify(src = null, method = rpcMethod, params = Some(Right(params.result)))
//     }

//     null
//   }
// }


object LabradRpc {
  /**
   * Make a map of json rpc Handlers for all methods annotated with @Call on the
   * given instance. There must be instances of Reads and Writes in scope for
   * parameter types and return values, respectively. Note that this uses a
   * macro implementation so that the type analysis and implicit resolution
   * actually happens at compile time, rather than runtime.
   */
  def makeHandlers[T](inst: T): Map[Long, Handler] = macro makeHandlersImpl[T]

  /**
   * Make a map of json rpc Handlers for all methods annotated with @Call on the
   * given instance. There must be instances of Reads and Writes in scope for
   * parameter types and return values, respectively. Note that this uses a
   * macro implementation so that the type analysis and implicit resolution
   * actually happens at compile time, rather than runtime.
   */
  //def makeProxy[T](cls: Class[T], endpoint: Endpoint): T = macro makeProxyImpl[T]

  def makeHandlersImpl[T: c.WeakTypeTag](c: MacroContext)(inst: c.Expr[T]): c.Expr[Map[Long, Handler]] = {
    import c.universe._

    // fully-qualified symbols and types (for hygiene)
    val map = q"_root_.scala.collection.immutable.Map"
    val seq = q"_root_.scala.Seq"
    val dataT = tq"_root_.org.labrad.browser.data.Data"
    val ex = q"_root_.org.labrad.LabradRpc.extractArg"

    // extract information from an optional @Call annotation on a symbol
    implicit class RichAnnotation(a: Annotation) {
      // extract a single parameter by name
      def param(name: String): Option[Constant] = {
        a.tree.children.tail.collectFirst {
          case AssignOrNamedArg(Ident(TermName(`name`)), Literal(c)) => c
        }
      }
    }

    def settingAnnotation(s: Symbol): Option[(Long, String, String)] =
      for {
        a <- s.annotations.find(_.tree.tpe =:= typeOf[Setting])
        id <- a.param("id").map { case Constant(value: Long) => value }
        name <- a.param("name").map { case Constant(value: String) => value }
        doc = a.param("doc").map { case Constant(value: String) => value }.getOrElse("")
      } yield (id, name, doc)

    // create a handler to dispatch rpc requests to a specific method
    def methodHandler(m: MethodSymbol, defaultMethods: Seq[MethodSymbol]): c.Tree = {
      val methodName = m.name.decodedName.toString

      val (paramTypes, returnType) = m.typeSignature match {
        case PolyType(args, ret) => (args.map(_.asType), ret)
        case MethodType(args, ret) => (args.map(_.asTerm), ret)
        case NullaryMethodType(ret) => (Nil, ret)
      }
      val paramNames = paramTypes.map(_.name.decodedName.toString)

      // selector for this method
      val f = Select(inst.tree, TermName(methodName))

      // create arguments for extractArg for a particular parameter
      def p(t: Symbol, i: Int): Seq[Tree] = {
        val name = t.name.decodedName.toString
        val getter = inferGetter(t.pos, t.typeSignature.dealias)
        val defaultName = methodName + "$default$" + (i+1)
        val default = defaultMethods.find(_.name.decodedName.toString == defaultName) match {
          case Some(m) =>
            val invokeDefault = Apply(
              Select(
                inst.tree,
                TermName(defaultName)
              ),
              List()
            )
            q"_root_.scala.Some(() => $invokeDefault)"

          case None => q"_root_.scala.None"
        }

        Seq(q"$i", q"$name", getter, default)
      }

      val invoke = paramTypes match {
        case Seq()                                       => q"""(in: $dataT) => $f()"""
        case Seq(t0)                                     => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}))"""
        case Seq(t0, t1)                                 => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}))"""
        case Seq(t0, t1, t2)                             => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}))"""
        case Seq(t0, t1, t2, t3)                         => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}))"""
        case Seq(t0, t1, t2, t3, t4)                     => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}))"""
        case Seq(t0, t1, t2, t3, t4, t5)                 => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}), $ex(in, ..${p(t5,5)}))"""
        case Seq(t0, t1, t2, t3, t4, t5, t6)             => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}), $ex(in, ..${p(t5,5)}), $ex(in, ..${p(t6,6)}))"""
        case Seq(t0, t1, t2, t3, t4, t5, t6, t7)         => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}), $ex(in, ..${p(t5,5)}), $ex(in, ..${p(t6,6)}), $ex(in, ..${p(t7,7)}))"""
        case Seq(t0, t1, t2, t3, t4, t5, t6, t7, t8)     => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}), $ex(in, ..${p(t5,5)}), $ex(in, ..${p(t6,6)}), $ex(in, ..${p(t7,7)}), $ex(in, ..${p(t8,8)}))"""
        case Seq(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9) => q"""(in: $dataT) => $f($ex(in, ..${p(t0,0)}), $ex(in, ..${p(t1,1)}), $ex(in, ..${p(t2,2)}), $ex(in, ..${p(t3,3)}), $ex(in, ..${p(t4,4)}), $ex(in, ..${p(t5,5)}), $ex(in, ..${p(t6,6)}), $ex(in, ..${p(t7,7)}), $ex(in, ..${p(t8,8)}), $ex(in, ..${p(t9,9)}))"""
      }

      // get return value writer, and determine whether the method is async
      val (resultSetter, async) = returnType.dealias match {
        case t if t <:< c.typeOf[Future[Any]] =>
          val resultTpe = typeParams(lub(t :: c.typeOf[Future[Nothing]] :: Nil))(0)
          (inferSetter(m.pos, resultTpe), true)

        case t => (inferSetter(m.pos, t), false)
      }

      if (async) {
        q"""new _root_.org.labrad.browser.jsonrpc.AsyncMethodHandler($methodName, $seq(..$paramNames), $invoke, $resultSetter)"""
      } else {
        q"""new _root_.org.labrad.browser.jsonrpc.SyncMethodHandler($methodName, $seq(..$paramNames), $invoke, $resultSetter)"""
      }
    }

    // return a list of type parameters in the given type
    // example: List[(String, Int)] => Seq(Tuple2, String, Int)
    def typeParams(tpe: Type): Seq[Type] = {
      val b = Iterable.newBuilder[Type]
      tpe.foreach(b += _)
      b.result.drop(2).grouped(2).map(_.head).toIndexedSeq
    }

    // locate an implicit Getter[T] for the given type
    def inferGetter(pos: Position, t: Type): Tree = {
      val readerTpe = appliedType(c.typeOf[Getter[_]], List(t))
      c.inferImplicitValue(readerTpe) match {
        case EmptyTree => c.abort(pos, s"could not find implicit value of type Getter[$t]")
        case tree => tree
      }
    }

    // locate an implicit Setter[T] for the given type
    def inferSetter(pos: Position, t: Type): Tree = {
      val writerTpe = appliedType(c.typeOf[Setter[_]], List(t))
      c.inferImplicitValue(writerTpe) match {
        case EmptyTree => c.abort(pos, s"could not find implicit value of type Setter[$t]")
        case tree => tree
      }
    }


    val t = weakTypeOf[T]
    val methods = t.members.toSeq.collect { case m: MethodSymbol => m }
    val defaults = methods.filter(_.name.decodedName.toString.contains("$default$"))

    // build handler for each annotated method
    val handlers = for {
      method <- methods
      (id, name, doc) <- settingAnnotation(method)
    } yield {
      val handler = methodHandler(method, defaults)
      q"""$id -> $handler"""
    }

    // final result is a map from rpc names to handlers
    c.Expr[Map[Long, Handler]](q"""$map(..$handlers)""")
  }

  def extractArg[A](data: Data, i: Int, name: String, getter: Getter[A], default: Option[() => A]): A = {
    if (data.clusterSize >= i+1) {
      val param = data(i)
      getter.get(param)
      // sys.error("unable to convert $param")
    } else {
      default match {
        case Some(f) => f()
        case None => sys.error(s"missing required param $name ($i)")
      }
    }
  }

  // def makeProxyImpl[T: c.WeakTypeTag](c: Context)(cls: c.Expr[Class[T]], endpoint: c.Expr[Endpoint]): c.Expr[T] = {
  //   import c.universe._

  //   // fully-qualified symbols and types (for hygiene)
  //   val map = q"_root_.scala.collection.immutable.Map"
  //   val seq = q"_root_.scala.Seq"

  //   // extract information from an optional @Setting annotation on a symbol
  //   implicit class RichAnnotation(a: Annotation) {
  //     // extract a single parameter by name
  //     def param(name: String): Option[Constant] = {
  //       a.tree.children.tail.collectFirst {
  //         case AssignOrNamedArg(Ident(TermName(`name`)), Literal(c)) => c
  //       }
  //     }
  //   }

  //   def rpcAnnotation(s: Symbol): Option[String] =
  //     for {
  //       a <- s.annotations.find(_.tree.tpe =:= typeOf[Call])
  //       value <- a.param("value").map { case Constant(value: String) => value }
  //     } yield value

  //   def notifyAnnotation(s: Symbol): Option[String] =
  //     for {
  //       a <- s.annotations.find(_.tree.tpe =:= typeOf[Notify])
  //       value <- a.param("value").map { case Constant(value: String) => value }
  //     } yield value

  //   // create a handler to dispatch rpc requests to a specific method
  //   def paramDefs(m: MethodSymbol): c.Tree = {
  //     val methodName = m.name.decodedName.toString

  //     val (paramTypes, returnType) = m.typeSignature match {
  //       case PolyType(args, ret) => (args.map(_.asType), ret)
  //       case MethodType(args, ret) => (args.map(_.asTerm), ret)
  //       case NullaryMethodType(ret) => (Nil, ret)
  //     }
  //     val paramNames = paramTypes.map(_.name.decodedName.toString)
  //     val writers = paramTypes.map(t => inferWriter(t.pos, t.typeSignature.dealias))

  //     val params = paramTypes.map { t =>
  //       val paramName = t.name.decodedName.toString
  //       val writer = inferWriter(t.pos, t.typeSignature.dealias)
  //       q"($paramName, $writer)"
  //     }

  //     // get return value writer, and determine whether the method is async
  //     val resultWriterOpt = returnType.dealias match {
  //       case t if t <:< c.typeOf[Future[Any]] =>
  //         val resultTpe = typeParams(lub(t :: c.typeOf[Future[Nothing]] :: Nil))(0)
  //         Some(inferWriter(m.pos, resultTpe))

  //       case t =>
  //         None
  //     }

  //     q"$seq(..$params)"
  //   }

  //   // create a handler to dispatch rpc requests to a specific method
  //   def resultReader(m: MethodSymbol): c.Tree = {
  //     val methodName = m.name.decodedName.toString

  //     val returnType = m.typeSignature match {
  //       case PolyType(args, ret) => ret
  //       case MethodType(args, ret) => ret
  //       case NullaryMethodType(ret) => ret
  //     }

  //     // get return value writer, and determine whether the method is async
  //     returnType.dealias match {
  //       case t if t <:< c.typeOf[Future[Any]] =>
  //         val resultTpe = typeParams(lub(t :: c.typeOf[Future[Nothing]] :: Nil))(0)
  //         inferReader(m.pos, resultTpe)

  //       case t =>
  //         sys.error(s"@Call method $methodName must return a Future")
  //     }
  //   }

  //   def checkUnitReturn(m: MethodSymbol): Unit = {
  //     val methodName = m.name.decodedName.toString

  //     val returnType = m.typeSignature match {
  //       case PolyType(args, ret) => ret
  //       case MethodType(args, ret) => ret
  //       case NullaryMethodType(ret) => ret
  //     }

  //     // get return value writer, and determine whether the method is async
  //     returnType.dealias match {
  //       case t if t <:< c.typeOf[Unit] =>
  //       case t =>
  //         sys.error(s"@Notify method $methodName must return Unit")
  //     }
  //   }

  //   // return a list of type parameters in the given type
  //   // example: List[(String, Int)] => Seq(Tuple2, String, Int)
  //   def typeParams(tpe: Type): Seq[Type] = {
  //     val b = Iterable.newBuilder[Type]
  //     tpe.foreach(b += _)
  //     b.result.drop(2).grouped(2).map(_.head).toIndexedSeq
  //   }

  //   // locate an implicit Reads[T] for the given type
  //   def inferReader(pos: Position, t: Type): Tree = {
  //     val readerTpe = appliedType(c.typeOf[Reads[_]], List(t))
  //     c.inferImplicitValue(readerTpe) match {
  //       case EmptyTree => c.abort(pos, s"could not find implicit value of type Reads[$t]")
  //       case tree => tree
  //     }
  //   }

  //   // locate an implicit Writes[T] for the given type
  //   def inferWriter(pos: Position, t: Type): Tree = {
  //     val writerTpe = appliedType(c.typeOf[Writes[_]], List(t))
  //     c.inferImplicitValue(writerTpe) match {
  //       case EmptyTree => c.abort(pos, s"could not find implicit value of type Writes[$t]")
  //       case tree => tree
  //     }
  //   }

  //   val t = weakTypeOf[T]
  //   val methods = t.members.toSeq.collect { case m: MethodSymbol => m }

  //   // build handler for each annotated method
  //   val calls = for {
  //     method <- methods
  //     methodName = method.name.decodedName.toString
  //     rpcName <- rpcAnnotation(method)
  //   } yield {
  //     q"($methodName, ($rpcName, ${paramDefs(method)}, ${resultReader(method)}))"
  //   }

  //   val notifys = for {
  //     method <- methods
  //     methodName = method.name.decodedName.toString
  //     notifyName <- notifyAnnotation(method)
  //   } yield {
  //     checkUnitReturn(method)
  //     q"($methodName, ($notifyName, ${paramDefs(method)}))"
  //   }

  //   c.Expr[T](q"""
  //     _root_.java.lang.reflect.Proxy.newProxyInstance(
  //       $cls.getClassLoader(),
  //       Array($cls),
  //       new EndpointInvocationHandler($endpoint, $map(..$calls), $map(..$notifys))
  //     ).asInstanceOf[$t]
  //   """)
  // }
}
