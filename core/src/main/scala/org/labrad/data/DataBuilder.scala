package org.labrad.data

import io.netty.buffer.{ByteBuf, Unpooled}
import java.nio.ByteOrder
import java.nio.ByteOrder._
import java.nio.charset.StandardCharsets.UTF_8
import org.labrad.types._
import scala.collection.mutable

object DataBuilder {
  def apply(tag: String): DataBuilder = apply(Type(tag))
  def apply(t: Type = null)(implicit bo: ByteOrder = BIG_ENDIAN): DataBuilder = new DataBuilder(Option(t))
}

class DataBuilder(tOpt: Option[Type] = None)(implicit byteOrder: ByteOrder = BIG_ENDIAN) { self =>

  private val buf = Unpooled.buffer().order(byteOrder)

  private var state: State = Start(tOpt).call()

  override def toString: String = {
    s"DataBuilder(state = $state, bytes = ${buf.readableBytes})"
  }

  sealed trait State {
    def caller: State
    def call(): State
    def resume(t: Type): State

    override def toString: String = {
      var state = this
      var s = render("")
      while (state.caller != state) {
        state = state.caller
        s = state.render(s)
      }
      s
    }
    def render(subState: String): String
  }

  case class Start(tOpt: Option[Type]) extends State {
    def caller: State = this
    def call(): State = tOpt match {
      case None => ConsumeAny(caller = this).call()
      case Some(t) => consume(t, caller = this).call()
    }
    def resume(t: Type): State = Done(t)
    override def render(subState: String): String = subState
  }

  case class Done(t: Type) extends State {
    def caller: State = this
    def call(): State = ???
    def resume(t: Type): State = ???
    override def render(subState: String): String = s"Done($t)"
  }

  private def consume(t: Type, caller: State): State = {
    t match {
      case TArr(elem, depth) => ConsumeArray(elem, depth, caller)
      case TCluster(elems @ _*) => ConsumeCluster(elems, caller)
      case TError(payload) => ConsumeError(payload, caller)
      case t => ConsumeOne(t, caller)
    }
  }

  case class ConsumeAny(caller: State) extends State {
    def call(): State = this
    def resume(t: Type): State = ???
    def gotValue(t: Type): State = caller.resume(t)
    override def render(subState: String): String = "<?>"
  }

  case class ConsumeOne(t: Type, caller: State) extends State {
    def call(): State = this
    def resume(t: Type): State = ???
    def gotValue(): State = caller.resume(t)
    override def render(subState: String): String = s"<$t>"
  }

  case class ConsumeClusterAny(size: Int, caller: State) extends State {
    private val consumer = ConsumeAny(caller = this)
    private var i = 0
    private var elems: mutable.Buffer[Type] = _

    def call(): State = {
      i = 0
      elems = mutable.Buffer.empty[Type]
      consumer.call()
    }

    def resume(t: Type): State = {
      i += 1
      elems += t
      if (i < size) {
        consumer.call()
      } else {
        caller.resume(TCluster(elems: _*))
      }
    }

    override def render(subState: String): String = {
      val elemStrs = for (idx <- 0 until size) yield {
        if (idx == i) {
          subState
        } else if (idx < elems.length) {
          elems(idx).toString
        } else {
          "?"
        }
      }
      s"(${elemStrs.mkString})"
    }
  }

  case class ConsumeCluster(elems: Seq[Type], caller: State) extends State {
    val size = elems.length

    private val consumers = elems.toArray.map(t => consume(t, caller = this))
    private var i = 0
    private val t = TCluster(elems: _*)

    def call(): State = {
      i = 0
      this
    }

    def resume(t: Type): State = {
      i += 1
      if (i < size) {
        consumers(i).call()
      } else {
        caller.resume(t)
      }
    }

    def gotSize(): State = {
      consumers(i).call()
    }

    override def render(subState: String): String = {
      val elemStrs = for ((elem, idx) <- elems.zipWithIndex) yield {
        if (idx == i) {
          subState
        } else {
          elem.toString
        }
      }
      s"(${elemStrs.mkString})"
    }
  }

  case class ConsumeArrayAny(shape: Array[Int], caller: State) extends State {

    require(shape.length >= 1, s"expecting shape of length at least 1.")
    require(shape.forall(_ >= 0), s"dimensions must be nonnegative, got ${shape.mkString(",")}")

    private val depth = shape.length
    private val size: Int = shape.product

    private var i: Int = 0
    private var elem: Type = null
    private val anyConsumer = ConsumeAny(caller = this)
    private var consumer: State = null

    def call(): State = {
      for (dim <- shape) {
        buf.writeInt(dim)
      }
      i = 0
      anyConsumer.call()
    }

    def resume(t: Type): State = {
      if (i == 0) {
        elem = t
        consumer = consume(elem, caller = this)
      }
      i += 1
      if (i < size) {
        consumer.call()
      } else {
        caller.resume(TArr(elem, depth))
      }
    }

    override def render(subState: String): String = {
      val dStr = if (depth == 1) "" else depth.toString
      s"*$dStr{$i}$subState"
    }
  }

  case class ConsumeArray(elem: Type, depth: Int, caller: State) extends State {
    private val consumer = consume(elem, caller = this)
    private var shape: Array[Int] = null
    private var size: Int = 0
    private var i: Int = 0

    def call(): State = {
      shape = null
      i = 0
      this
    }

    def resume(t: Type): State = {
      i += 1
      if (i < size) {
        consumer.call()
      } else {
        caller.resume(TArr(elem, depth))
      }
    }

    def gotShape(shape: Array[Int]): State = {
      require(shape.length == depth, s"expecting shape of depth $depth, got ${shape.length}")
      require(shape.forall(_ >= 0), s"dimensions must be nonnegative, got ${shape.mkString(",")}")
      this.shape = shape
      this.size = shape.product
      for (dim <- shape) {
        buf.writeInt(dim)
      }
      consumer.call()
    }

    override def render(subState: String): String = {
      val dStr = if (depth == 1) "" else depth.toString
      if (shape == null) {
        s"*$dStr<shape>$elem"
      } else {
        s"*$dStr{$i}$subState"
      }
    }
  }

  case class ConsumeError(payload: Type, caller: State) extends State {
    private val consumer = ConsumeOne(payload, caller = this)
    private var awaitingError = true

    def call(): State = {
      this
    }

    def resume(t: Type): State = {
      caller.resume(TError(t))
    }

    def gotError(code: Int, message: String): State = {
      buf.writeInt(code)
      writeString(message)
      awaitingError = false
      consumer.call()
    }

    override def render(subState: String): String = {
      if (awaitingError) {
        s"<E>$payload"
      } else {
        s"E$subState"
      }
    }
  }

  case class ConsumeErrorPayload(code: Int, message: String, caller: State) extends State {
    private val consumer = ConsumeAny(caller = this)
    private var i = 0

    def call(): State = {
      buf.writeInt(code)
      writeString(message)
      consumer.call()
    }

    def resume(t: Type): State = {
      caller.resume(TError(t))
    }

    override def render(subState: String): String = {
      s"E$subState"
    }
  }

  private def nope(msg: String) = {
    throw new IllegalStateException(msg)
  }

  private def writeBytes(bytes: Array[Byte]): Unit = {
    buf.writeInt(bytes.length)
    buf.writeBytes(bytes)
  }

  private def writeString(s: String): Unit = {
    val bytes = s.getBytes(UTF_8)
    writeBytes(bytes)
  }

  private def addSimple(t: Type)(write: => Unit): this.type = {
    state = state match {
      case state: ConsumeAny => write; state.gotValue(t)
      case state: ConsumeOne if t == state.t => write; state.gotValue()
      case state: ConsumeOne => nope(s"cannot add $t. expecting ${state.t}")
      case state: ConsumeArray => nope(s"cannot add $t. expecting array shape")
      case state => nope(s"cannot add $t. unexpected state: $state")
    }
    this
  }

  def none(): this.type = addSimple(TNone) { }
  def bool(x: Boolean): this.type = addSimple(TBool) { buf.writeBoolean(x) }
  def int(x: Int): this.type = addSimple(TInt) { buf.writeInt(x) }
  def uint(x: Long): this.type = addSimple(TUInt) { buf.writeInt(x.toInt) }
  def bytes(x: Array[Byte]): this.type = addSimple(TStr) { writeBytes(x) }
  def string(s: String): this.type = addSimple(TStr) { writeString(s) }
  def time(seconds: Long, fraction: Long): this.type = addSimple(TTime) { buf.writeLong(seconds); buf.writeLong(fraction) }
  def value(x: Double, unit: String = ""): this.type = addSimple(TValue(Some(unit))) { buf.writeDouble(x) }
  def complex(re: Double, im: Double, unit: String = ""): this.type = {
    addSimple(TComplex(Some(unit))) { buf.writeDouble(re); buf.writeDouble(im) }
  }

  def array(size: Int): this.type = array(Array(size))
  def array(shape: Int*): this.type = array(shape.toArray)
  def array(shape: Array[Int]): this.type = {
    state = state match {
      case state: ConsumeAny => ConsumeArrayAny(shape, caller = state.caller).call()
      case state: ConsumeArray => state.gotShape(shape)
      case state: ConsumeOne => nope(s"cannot add array. expecting ${state.t}")
      case state => nope(s"cannot add array. unexpected state: $state")
    }
    this
  }

  def cluster(size: Int): this.type = {
    state = state match {
      case state: ConsumeAny => ConsumeClusterAny(size, caller = state.caller).call()
      case state: ConsumeCluster if size == state.size => state.gotSize()
      case state: ConsumeCluster => nope(s"cannot add cluster of size $size. expecting ${state.size}")
      case state: ConsumeOne => nope(s"cannot add cluster. expecting ${state.t}")
      case state => nope(s"cannot add cluster. unexpected state: $state")
    }
    this
  }

  def error(code: Int, message: String): this.type = {
    state = state match {
      case state: ConsumeAny => ConsumeErrorPayload(code, message, caller = state.caller).call()
      case state: ConsumeError => state.gotError(code, message)
      case state => nope(s"cannot add error. unexpected state: $state")
    }
    this
  }

  def add(data: Data): this.type = {
    data.t match {
      case TNone => none()
      case TBool => bool(data.getBool)
      case TInt => int(data.getInt)
      case TUInt => uint(data.getUInt)
      case TTime => time(data.getSeconds, data.getFraction)
      case TStr => bytes(data.getBytes)
      case TValue(unitOpt) => value(data.getValue, unitOpt.getOrElse(""))
      case TComplex(unitOpt) => complex(data.getReal, data.getImag, unitOpt.getOrElse(""))
      case _: TArr =>
        array(data.arrayShape)
        for (elem <- data.flatIterator) {
          add(elem)
        }
      case _: TCluster =>
        cluster(data.clusterSize)
        for (elem <- data.clusterIterator) {
          add(elem)
        }
      case _: TError =>
        error(data.getErrorCode, data.getErrorMessage)
        add(data.getErrorPayload)
    }
    this
  }

  def build(): Data = {
    state match {
      case Done(t) => new FlatData(t, buf.toByteArray, 0)
      case state: ConsumeAny => nope("cannot build. expecting more data")
      case state: ConsumeArray => nope("cannot build. expecting array shape")
      case state: ConsumeOne => nope(s"cannot build. expecting ${state.t}")
      case state => nope(s"cannot build. unexpected state: $state")
    }
  }
}
