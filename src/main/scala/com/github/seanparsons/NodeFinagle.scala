package com.github.seanparsons

import scala.annotation.tailrec
import java.io.File
import org.apache.commons.io.FileUtils
import com.twitter.finagle.builder.{ServerBuilder, Server}
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}
import com.twitter.finagle.http.Http
import java.net.InetSocketAddress
import com.twitter.finagle.Service
import com.twitter.util.Eval
import scala.util.control.Exception._
import com.twitter.conversions.time._

case class ServerWithScript(server: Server, script: String)

object NodeFinagle extends App {
  val waitStepTime = 100L
  val carriageReturn = '\r'.toInt
  val reader = Console.in
  val scriptFile = new File("Script.nodefinagle")

  @tailrec def enterPressed: Boolean = {
    if (reader.ready()) {
      val character = reader.read()
      if (character == carriageReturn) {
        true
      } else {
        enterPressed
      }
    } else {
      false
    }
  }

  @tailrec def pauseFor(time: Long): Boolean = {
    if (time <= 0) {
      true
    } else {
      if (enterPressed) {
        false
      } else {
        Thread.sleep(waitStepTime)
        pauseFor(time - waitStepTime)
      }
    }
  }

  def parseScript(script: String): Option[Service[HttpRequest, HttpResponse]] = {
    allCatch.opt(new Eval().apply[Service[HttpRequest, HttpResponse]](script))
  }

  def createServer(service: Service[HttpRequest, HttpResponse]): Server = {
    ServerBuilder()
      .codec(Http.get())
      .bindTo(new InetSocketAddress(8080))
      .name("HttpServer")
      .build(service)
  }


  def loadScript(scriptPath: File): Option[String] = {
    if (scriptPath.exists && scriptPath.isFile) {
      Some(FileUtils.readFileToString(scriptPath))
    } else {
      None
    }
  }

  @tailrec def runServer(prior: Option[ServerWithScript]): Unit = {
    val loaded = loadScript(scriptFile)
    // If loaded file is different to previous file.
    val serverWithScript = if (prior.map(_.script) != loaded) {
      // Close old server.
      prior.foreach(_.server.close())
      // Spin up new server.
      loaded.flatMap{script =>
        println("Loaded script:")
        println(script)
        parseScript(script)
          .map(createServer)
          .map(server => new ServerWithScript(server, script))
      }
    } else {
      prior
    }
    // Pause for a second, checking the user doesn't want this to exit.
    if (pauseFor(1000L)){
      // If they're happy, run this again.
      runServer(serverWithScript)
    } else {
      serverWithScript.foreach(_.server.close())
    }
  }

  runServer(None)
}