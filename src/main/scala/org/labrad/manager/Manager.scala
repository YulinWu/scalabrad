package org.labrad.manager

import io.netty.handler.ssl.{SslContext, SslContextBuilder}
import io.netty.handler.ssl.util.SelfSignedCertificate
import io.netty.util.DomainNameMapping
import java.io.File
import java.net.URI
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import java.nio.file.Files
import org.labrad.annotations._
import org.labrad.data._
import org.labrad.errors._
import org.labrad.registry._
import org.labrad.util._
import org.labrad.util.Paths._
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


trait AuthService {
  def authenticate(challenge: Array[Byte], response: Array[Byte]): Boolean
}

class AuthServiceImpl(password: Array[Char]) extends AuthService {
  def authenticate(challenge: Array[Byte], response: Array[Byte]): Boolean = {
    val md = MessageDigest.getInstance("MD5")
    md.update(challenge)
    md.update(UTF_8.encode(CharBuffer.wrap(password)))
    val expected = md.digest
    var same = expected.length == response.length
    for ((a, b) <- expected zip response) same = same & (a == b)
    same
  }
}


class CentralNode(port: Int, tlsPort: Int, password: Array[Char], storeOpt: Option[RegistryStore], tlsConfig: TlsHostConfig, requireStartTls: Boolean) extends Logging {
  // start services
  val tracker = new StatsTrackerImpl
  val hub: Hub = new HubImpl(tracker, () => messager)
  val messager: Messager = new MessagerImpl(hub, tracker)
  val auth: AuthService = new AuthServiceImpl(password)

  // Manager gets id 1L
  tracker.connectServer(Manager.ID, Manager.NAME)

  for (store <- storeOpt) {
    // Registry gets id 2L
    val name = "Registry"
    val id = hub.allocateServerId(name)
    val server = new Registry(id, name, store, hub, tracker)
    hub.connectServer(id, name, server)
  }

  // start listening for incoming network connections
  val listener = new Listener(auth, hub, tracker, messager, port, tlsPort, tlsConfig, requireStartTls)

  def stop() {
    listener.stop()
  }
}


object Manager extends Logging {
  val ID = 1L
  val NAME = "Manager"
  val DOC = "Provides basic support for all labrad connections, including discovery of other servers and lookup of metadata about them."

  // setting ids
  val SERVERS = 1L
  val SETTINGS = 2L
  val LOOKUP = 3L

  // first client id
  val ClientIdStart = 1000000000L

  // named messages
  case class Connect(id: Long, name: String, isServer: Boolean) extends Message {
    def msgName: String = "Connect"
    def msgData: Data = Cluster(UInt(id), Str(name), Bool(isServer))
  }

  case class Disconnect(id: Long, name: String, isServer: Boolean) extends Message {
    def msgName: String = "Disconnect"
    def msgData: Data = Cluster(UInt(id), Str(name), Bool(isServer))
  }

  case class ConnectServer(id: Long, name: String) extends Message {
    def msgName: String = "Server Connect"
    def msgData: Data = Cluster(UInt(id), Str(name))
  }

  case class DisconnectServer(id: Long, name: String) extends Message {
    def msgName: String = "Server Disconnect"
    def msgData: Data = Cluster(UInt(id), Str(name))
  }

  case class ExpireAll(id: Long) extends Message {
    def msgName: String = "Expire All"
    def msgData: Data = UInt(id)
  }

  case class ExpireContext(ctx: Context) extends Message {
    def msgName: String = "Expire Context"
    def msgData: Data = ctx.toData
  }

  def main(args: Array[String]) {
    val options = Util.parseArgs(args, Seq("port", "password", "registry", "tls-required", "tls-port", "tls-hosts"))

    val port = options.get("port").orElse(sys.env.get("LABRADPORT")).map(_.toInt).getOrElse(7682)
    val tlsPort = options.get("tls-port").orElse(sys.env.get("LABRAD_TLS_PORT")).map(_.toInt).getOrElse(7643)

    val password = options.get("password").orElse(sys.env.get("LABRADPASSWORD")).getOrElse("").toCharArray
    val registryUri = options.get("registry").orElse(sys.env.get("LABRADREGISTRY")).map(new URI(_)).getOrElse {
      (sys.props("user.home") / ".labrad" / "registry.sqlite").toURI
    }
    val requireTls = options.get("tls-required").map(Util.parseBooleanOpt).getOrElse(true)
    val tlsConfig = parseTlsHostsConfig(options.get("tls-hosts").orElse(sys.env.get("LABRAD_TLS_HOSTS")).getOrElse(""))

    val storeOpt = registryUri.getScheme match {
      case null if registryUri == new URI("none") =>
        log.info("running with external registry")
        None

      case "file" =>
        val registry = new File(Util.bareUri(registryUri)).getAbsoluteFile

        val (store, format) = if (registry.isDirectory) {
          ensureDir(registry)
          registryUri.getQuery match {
            case null | "format=binary" => (new BinaryFileStore(registry), "binary")
            case "format=delphi" => (new DelphiFileStore(registry), "delphi")
            case query => sys.error(s"invalid format for registry directory: $query")
          }
        } else {
          ensureDir(registry.getParentFile)
          registryUri.getQuery match {
            case null | "format=sqlite" => (SQLiteStore(registry), "sqlite")
            case query => sys.error(s"invalid format for registry file: $query")
          }
        }
        log.info(s"registry location: $registry, format=$format")
        Some(store)

      case "labrad" =>
        val remoteHost = registryUri.getHost
        val remotePort = registryUri.getPort match {
          case -1 => 7682 // not specified; use default
          case port => port
        }
        val remotePassword = registryUri.getUserInfo match {
          case null => password
          case info => info.split(":") match {
            case Array() => password
            case Array(pw) => pw.toCharArray
            case Array(u, pw) => pw.toCharArray
          }
        }
        log.info(s"remote registry location: $remoteHost:$remotePort")
        Some(RemoteStore(remoteHost, remotePort, remotePassword))

      case scheme =>
        sys.error(s"unknown scheme for registry uri: $scheme. must use 'file', 'labrad'")
    }

    val centralNode = new CentralNode(port, tlsPort, password, storeOpt, tlsConfig, requireTls)

    // Optionally wait for EOF to stop the manager.
    // This is a convenience feature when developing in sbt, allowing the
    // manager to be stopped without killing sbt. However, this is generally
    // not desired when deployed; for example, start-stop-daemon detaches
    // from the process, so that stdin gets EOF, but we want the manager
    // to continue to run.
    val stopOnEOF = sys.props.get("org.labrad.stopOnEOF") == Some("true")
    if (stopOnEOF) {
      Util.awaitEOF()
      centralNode.stop()
    } else {
      sys.addShutdownHook {
        centralNode.stop()
      }
    }
  }

  /**
   * Parse a string representing the tls host configuration.
   *
   * The config is a semicolon-separated list of hosts, where for each host we have either just
   * <hostname> (in which case we will use self-signed certificates for that host), or else
   * <hostname>?cert=<cert-file>&key=<key-file>[&intermediates=<intermediates-file>].
   *
   * For example, if we had the string:
   *
   * public.com?cert=/etc/ssl/certs/public.crt?key=/etc/ssl/private/public.key;private;private2
   *
   * Then we would configure TLS to use the given certificates in /etc/ssl for connections made
   * to the hostname public.com, and our own self-signed certs for connections made to hostnames
   * private and private2.
   */
  def parseTlsHostsConfig(hostsConfig: String): TlsHostConfig = {
    val hosts = if (hostsConfig == "") Seq() else hostsConfig.split(";").toSeq.map(parseTlsHost)
    TlsHostConfig(sslContextForHost("localhost"), hosts: _*)
  }

  /**
   * Parse hostname config for a single TLS host.
   */
  def parseTlsHost(hostConfig: String): (String, (File, SslContext)) = {
    require(hostConfig != "")

    hostConfig.split('?') match {
      case Array(host) =>
        host -> sslContextForHost(host)

      case Array(host, paramStr) =>
        val params = paramStr.split('&').map { param =>
          param.split('=') match { case Array(k, v) => k -> v }
        }.toMap

        val unknownParams = params.keys.toSet -- Set("cert", "intermediates", "key")
        require(unknownParams.isEmpty, s"unknown parameters for host $host: ${unknownParams.mkString(", ")}")

        val certFileAndSslCtx = (params.get("cert"), params.get("intermediates"), params.get("key")) match {
          case (Some(cert), None, Some(key)) =>
            val certFile = new File(cert)
            val sslCtx = SslContextBuilder.forServer(certFile, new File(key)).build()
            (certFile, sslCtx)

          case (Some(cert), Some(interm), Some(key)) =>
            // concatenate the files containing our server certificate and intermediate certs
            val certFile = File.createTempFile("labrad-manager", "cert")
            Files.write(certFile.toPath,
              Files.readAllBytes(new File(cert).toPath) ++ Files.readAllBytes(new File(interm).toPath))
            certFile.deleteOnExit()
            val sslCtx = SslContextBuilder.forServer(certFile, new File(key)).build()
            (certFile, sslCtx)

          case (None, None, None) =>
            sslContextForHost(host)

          case _ =>
            sys.error(s"must specify both tls-cert-file and tls-key-file")
        }

        host -> certFileAndSslCtx
    }
  }

  /**
   * Create an SSL/TLS context for the given host, using self-signed certificates.
   */
  private def sslContextForHost(host: String): (File, SslContext) = {
    val certFile = sys.props("user.home") / ".labrad" / "manager" / "certs"/ s"${host}.cert"
    val keyFile = sys.props("user.home") / ".labrad" / "manager" / "keys" / s"${host}.key"

    if (!certFile.exists() || !keyFile.exists()) {
      // if one exists but not the other, we have a problem
      require(!certFile.exists(), s"found cert file $certFile but no matching key file $keyFile")
      require(!keyFile.exists(), s"found key file $keyFile but no matching cert file $certFile")

      log.info(s"Generating self-signed certificate for host '$host'. certFile=$certFile, keyFile=$keyFile")
      val ssc = new SelfSignedCertificate(host)
      copy(ssc.certificate, certFile)
      copy(ssc.privateKey, keyFile)
    } else {
      log.info(s"Using saved certificate for host '$host'. certFile=$certFile, keyFile=$keyFile")
    }

    (certFile, SslContextBuilder.forServer(certFile, keyFile).build())
  }

  /**
   * Copy the given source file to the specified destination, creating
   * destination directories as needed.
   */
  private def copy(src: File, dst: File): Unit = {
    ensureDir(dst.getParentFile)
    Files.copy(src.toPath, dst.toPath)
  }

  /**
   * Create directories as needed to ensure that the specified location exists.
   */
  private def ensureDir(dir: File): Unit = {
    if (!dir.exists) {
      val ok = dir.mkdirs()
      if (!ok) sys.error(s"failed to create registry directory: $dir")
    }
  }
}
