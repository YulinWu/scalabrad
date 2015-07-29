package org.labrad.manager

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel._
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.ssl.{SniHandler, SslContext}
import io.netty.util.DomainNameMapping
import org.labrad.PacketCodec
import org.labrad.util._
import scala.concurrent.ExecutionContext

/**
 * listens for incoming labrad network connections
 */
class Listener(auth: AuthService, hub: Hub, tracker: StatsTracker, messager: Messager, port: Int, tlsPort: Int, tlsConfig: TlsHostConfig, requireStartTls: Boolean)(implicit ec: ExecutionContext)
extends Logging {
  val bossGroup = new NioEventLoopGroup(1)
  val workerGroup = new NioEventLoopGroup()

  def bootServer(port: Int, useTls: Boolean) = {
    try {
      val b = new ServerBootstrap()
      b.group(bossGroup, workerGroup)
       .channel(classOf[NioServerSocketChannel])
       .childOption[java.lang.Boolean](ChannelOption.TCP_NODELAY, true)
       .childOption[java.lang.Boolean](ChannelOption.SO_KEEPALIVE, true)
       .childHandler(new ChannelInitializer[SocketChannel] {
         override def initChannel(ch: SocketChannel): Unit = {
           val p = ch.pipeline
           if (useTls) {
             p.addLast(new SniHandler(tlsConfig.sslCtxs))
           }
           p.addLast("packetCodec", new PacketCodec())
           p.addLast("loginHandler", new LoginHandler(auth, hub, tracker, messager, tlsConfig, tlsOnly = useTls, requireStartTls = requireStartTls))
         }
       })

      // Bind and start to accept incoming connections.
      b.bind(port).sync().channel
    } catch {
      case e: Exception =>
        stop()
        throw e
    }
  }

  val rawServerChannel = bootServer(port, useTls = false)
  val tlsServerChannel = bootServer(tlsPort, useTls = true)

  log.info(s"now accepting labrad connections on ports: raw=$port, tls=$tlsPort")

  def stop() {
    log.info("shutting down")
    try {
      if (rawServerChannel != null) {
        rawServerChannel.close()
        rawServerChannel.closeFuture.sync()
      }
      if (tlsServerChannel != null) {
        tlsServerChannel.close()
        tlsServerChannel.closeFuture.sync()
      }
    } finally {
      workerGroup.shutdownGracefully()
      bossGroup.shutdownGracefully()
    }
  }
}
