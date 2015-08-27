package org.labrad.manager

import io.netty.channel._
import io.netty.handler.ssl.{SslContext, SslHandler}
import io.netty.util.DomainNameMapping
import java.io.{ByteArrayOutputStream, File, FileInputStream}
import java.net.{InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import org.labrad.ContextCodec
import org.labrad.crypto.BigInts._
import org.labrad.data._
import org.labrad.errors._
import org.labrad.types._
import org.labrad.util._
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random


case class TlsHostConfig(certs: DomainNameMapping[String], sslCtxs: DomainNameMapping[SslContext])

object TlsHostConfig {
  def apply(default: (File, SslContext), hosts: (String, (File, SslContext))*): TlsHostConfig = {
    val (defaultCertFile, defaultSslCtx) = default
    val certMapping = new DomainNameMapping[String](readFile(defaultCertFile))
    val ctxMapping = new DomainNameMapping[SslContext](defaultSslCtx)

    for ((host, (certFile, sslCtx)) <- hosts) {
      certMapping.add(host, readFile(certFile))
      ctxMapping.add(host, sslCtx)
    }

    TlsHostConfig(certMapping, ctxMapping)
  }

  private def readFile(f: File): String = new String(Files.readAllBytes(f.toPath), UTF_8)
}


class LoginHandler(auth: AuthService, hub: Hub, tracker: StatsTracker, messager: Messager, tlsConfig: TlsHostConfig, tlsOnly: Boolean, requireStartTls: Boolean)(implicit ec: ExecutionContext)
extends SimpleChannelInboundHandler[Packet] with Logging {

  override def channelActive(ctx: ChannelHandlerContext): Unit = {
    ctx.channel.remoteAddress match {
      case address: InetSocketAddress =>
        localhost = address.getAddress == InetAddress.getLoopbackAddress
        log.info(s"remote address=${address.getAddress} (localhost=$localhost)")

      case _ =>
    }
  }

  override def channelRead0(ctx: ChannelHandlerContext, packet: Packet): Unit = {
    val Packet(req, target, context, records) = packet
    val resp = try {
      handle(ctx, packet)
    } catch {
      case ex: LabradException =>
        log.debug("error during login", ex)
        ex.toData
      case ex: Throwable =>
        log.debug("error during login", ex)
        Error(1, ex.toString)
    }
    val future = ctx.channel.writeAndFlush(Packet(-req, target, context, Seq(Record(0, resp))))
    if (resp.isError) future.addListener(ChannelFutureListener.CLOSE)
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, ex: Throwable): Unit = {
    log.error("exceptionCaught", ex)
    ctx.close()
  }

  // whether the connection originated from localhost (if so, we allow unencrypted connections)
  private var localhost: Boolean = false

  // whether the connection is secure (either because this is a tls-only channel or we
  // have upgraded with STARTTLS to a secure connection.
  private var secure: Boolean = tlsOnly

  // challenge use for old-style password auth
  private val challenge = Array.ofDim[Byte](256)
  Random.nextBytes(challenge)

  // state for secure remote password (SRP) auth
  private var A: BigInt = _
  private var B: BigInt = _
  private var S: BigInt = _

  // whether to use srp instead of old-style password auth
  private var srp: Boolean = false

  private var handle: (ChannelHandlerContext, Packet) => Data =
    if (tlsOnly) handleLogin else handleStartTls

  private def handleStartTls(ctx: ChannelHandlerContext, packet: Packet): Data = packet match {
    case Packet(req, 1, _, Seq(Record(1, Cluster(Str("STARTTLS"), Str(host))))) =>
      val sslContext = tlsConfig.sslCtxs.map(host)
      val engine = sslContext.newEngine(ctx.alloc())
      val sslHandler = new SslHandler(engine, true)
      ctx.pipeline.addFirst(sslHandler)
      secure = true // now upgraded to TLS
      handle = handleLogin
      Str(tlsConfig.certs.map(host))

    case _ =>
      if (localhost || !requireStartTls) {
        handleLogin(ctx, packet)
      } else {
        throw LabradException(2, "Expected STARTTLS")
      }
  }

  private def handleLogin(ctx: ChannelHandlerContext, packet: Packet): Data = packet match {
    case Packet(req, 1, _, Seq()) if req > 0 =>
      srp = false
      handle = handleChallengeResponse
      Bytes(challenge)

    case Packet(req, 1, _, Seq(Record(0, Cluster(Str(ident), Bytes(aBytes))))) =>
      A = fromUnsignedByteArray(aBytes)
      val (salt, b, s) = auth.srpInit(ident, A)
      B = b
      S = s
      srp = true
      handle = handleChallengeResponse
      Cluster(Bytes(salt), Bytes(B.toUnsignedByteArray))

    case Packet(req, 1, _, Seq(Record(2, Str("PING")))) =>
      Str("PONG")

    case _ =>
      throw LabradException(1, "Invalid login packet")
  }

  private def handleChallengeResponse(ctx: ChannelHandlerContext, packet: Packet): Data = packet match {
    case Packet(req, 1, _, Seq(Record(0, Bytes(response)))) if req > 0 =>
      val data = if (srp) {
        val M2 = auth.srpAuthenticate(A, B, S, response).getOrElse { throw LabradException(2, "Incorrect password") }
        Cluster(Str("LabRAD 2.0"), Bytes(M2))
      } else {
        if (!auth.authenticate(challenge, response)) throw LabradException(2, "Incorrect password")
        Str("LabRAD 2.0")
      }
      handle = handleIdentification
      data
    case _ =>
      throw LabradException(1, "Invalid authentication packet")
  }

  private def handleIdentification(ctx: ChannelHandlerContext, packet: Packet): Data = packet match {
    case Packet(req, 1, _, Seq(Record(0, data))) if req > 0 =>
      val (handler, id) = data match {
        case Cluster(UInt(ver), Str(name)) =>
          val id = hub.allocateClientId(name)
          val handler = new ClientHandler(hub, tracker, messager, ctx.channel, id, name)
          hub.connectClient(id, name, handler)
          (handler, id)

        case Cluster(UInt(ver), Str(name), Str(doc)) =>
          val id = hub.allocateServerId(name)
          val handler = new ServerHandler(hub, tracker, messager, ctx.channel, id, name, doc)
          hub.connectServer(id, name, handler)
          (handler, id)

        // TODO: remove this case (collapse doc and notes)
        case Cluster(UInt(ver), Str(name), Str(docOrig), Str(notes)) =>
          val doc = if (notes.isEmpty) docOrig else (docOrig + "\n\n" + notes)
          val id = hub.allocateServerId(name)
          val handler = new ServerHandler(hub, tracker, messager, ctx.channel, id, name, doc)
          hub.connectServer(id, name, handler)
          (handler, id)

        case _ =>
          throw LabradException(1, "Invalid identification packet")
      }

      // logged in successfully; add new handler to channel pipeline
      val pipeline = ctx.pipeline
      pipeline.addLast("contextCodec", new ContextCodec(id))
      pipeline.addLast(handler.getClass.toString, handler)
      pipeline.remove(this)

      UInt(id)

    case _ =>
      throw LabradException(1, "Invalid identification packet")
  }
}
