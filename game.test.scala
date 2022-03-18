//> using lib "com.lihaoyi::utest:0.7.10"
//> using lib "io.undertow:undertow-core:2.2.3.Final"

import scalatags.Text.all.*
import utest._
import requests.RequestAuth

import io.undertow.Undertow
import java.util.Base64
import java.nio.charset.StandardCharsets
import java.net.{URLEncoder, URLDecoder}

object GameTests extends TestSuite {
  val auth: Option[RequestAuth] = None

  def withServer[T](example: cask.main.Main, endpoint: String)(f: String => T): T = {
    val server = Undertow.builder
      .addHttpListener(8081, "localhost")
      .setHandler(example.defaultHandler)
      .build
    server.start()
    val res =
      try f(s"http://localhost:8081/${endpoint}")
      finally server.stop()
    res
  }

  val tests = Tests {
    test("CaskHttpServer") {
      CaskHttpServer.authCode = None

      test("hello") - withServer(CaskHttpServer, "/") { host =>
        val body = requests.get(host).text()
        assert(
          body.contains("<title>Start</title>"),
          body.contains("<h1>Welcome</h1>"),
          body.contains("<div>Please <a href="),
          body.contains(">log in</a> to play.</div>")
        )
      }
      test("start") {
        test("failed") - withServer(CaskHttpServer, "/start?code=aaa") { host =>
          try {
            val body = requests.get(host).text()
          } catch {
            case e: requests.RequestFailedException =>
              e
          }
        }
      }
      test("shutdown") - withServer(CaskHttpServer, "/shutdown") { host =>
        val resp = requests.get(s"$host/", check = false)
        resp.statusCode ==> 200
        resp.history.get.statusCode ==> 301
      }
      test("run") {
        test("failed") - withServer(CaskHttpServer, "/run") { host =>
          // CaskHttpServer.authCode = auth
          val body = requests.get(host).text()
          assert(
            body.contains("<title>Authorization failed</title>"),
            body.contains("<h1>Authorization failed</h1>"),
            body.contains("<div>Please try <a href="),
            body.contains(">log in</a> again to play.</div>")
          )
        }

        test("passed") - withServer(CaskHttpServer, "/run") { host =>
          CaskHttpServer.authCode = Some(RequestAuth.Basic("aaa", "aaa"))
          try {
            val body = requests.get(host).text()
          } catch {
            case e: requests.RequestFailedException => ()
          }
        }
      }
      test("login") - withServer(CaskHttpServer, "/login") { host =>
        val body = requests.get(host).text()
        assert(
          body.contains("<title>Logowanie - Spotify</title>")
        )
      }

      val guess = "Bohemian%20Rhapsody"
      val answer = "Queen"
      val answer_enc = Base64.getEncoder.encodeToString(answer.getBytes(StandardCharsets.UTF_8))
      test("submit") {
        test("wrong_answer") - withServer(CaskHttpServer, s"/submit") { host =>

          val response = requests.post(
            s"$host",
            data = Seq("guess" -> s"$guess", "answer" -> s"$answer_enc")
          )

          val body = response.text()
          assert(
            body.contains("<title>Your results...</title>"),
            body.contains("<h1>Whoops...</h1>"),
            body.contains(s"<div>The correct answer was $answer. <a href=\"/run\">Another go?</a>")
          )
        }
      }
      test("submit") {
        test("right_answer") - withServer(CaskHttpServer, s"/submit") { host =>
          val response = requests.post(
            s"$host",
            data = Seq("guess" -> s"$answer", "answer" -> s"$answer_enc")
          )

          val body = response.text()
          assert(
            body.contains("<title>Your results...</title>"),
            body.contains("<h1>Well done!</h1>"),
            body.contains(s"<div>The correct answer was $answer. <a href=\"/run\">Another go?</a>")
          )
        }
      }
    }

    test("Spotify") {
      test("clientCredentials") {
        val auth_CC: RequestAuth = Spotify.clientCredentials()
        auth_CC
      }
      test("authorizationCode") {
        test("invalid_Code") {
          try {
            val auth = Spotify.authorizationCode("aaa")
          } catch {
            case e: requests.RequestFailedException =>
              e
          }
        }
      }
      test("loginUrl") {
        val loginUrl = Spotify.loginUrl
        assert(
          loginUrl == "https://accounts.spotify.com/authorize?client_id=1588c59aabee43ca9f4d30d5695a4a0c&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fstart&scope=user-read-playback-state+user-modify-playback-state"
        )
      }
    }
  }

}
