import scalatags.Text.all.*

import io.undertow.Undertow
import java.util.Base64
import java.nio.charset.StandardCharsets
import munit.Assertions
import munit.TestOptions
import sttp.client4.quick.*
import sttp.model.*

import Spotify.*

class ServerTest extends munit.FunSuite {
  type Endpoint = String
  def usingServer[T](impl: cask.main.Main)(f: Endpoint => T): T = {
    val server = Undertow.builder
      .addHttpListener(8081, "localhost")
      .setHandler(impl.defaultHandler)
      .build
    server.start()
    try f("http://localhost:8081/")
    finally server.stop()
  }

  object TestRepository extends Repository()(using Auth.BearerToken("unused")) {
    final val dummyPlaylistRef = PlaylistRef("uri://", "Test playlist", "1234567")
    override def listUserPlaylists(user: String): List[PlaylistRef] = List(dummyPlaylistRef)
    override def listPlaylist(playlist: PlaylistRef): List[Track] = playlist match {
      case `dummyPlaylistRef` =>
        val ScalaPodcast1 = Album("Scala for Fun and Profit", List(Artist("Julien Truffaut")))
        List(
          Track(ScalaPodcast1, "Simple Scala")
          // Track(ScalaPodcast1, "Game development in Scala.js"),
          // Track(ScalaPodcast1, "Music DSL in Scala.js")
        )
    }
  }
  inline def testUsingServer(options: TestOptions, server: cask.main.Main = new CaskHttpServer(TestRepository))(
      inline body: Endpoint => Any
  ) =
    test(options) {
      usingServer(server)(body)
    }

  testUsingServer("default page") { host =>
    val response = quickRequest.get(uri"$host/").send()
    val body = response.body
    body match
      case s"${_}<head>${head}</head><body>${body}</body>${_}" =>
        assertNoDiff(head, "<title>Guess the song!</title>")
        body match {
          case s"""<div>Please click <a href="${link}">here</a> to login.</div>""" =>
            assertNoDiff(link.replace("&amp;", "&"), Spotify.Auth.AuthorizationUrl)
        }
  }

  testUsingServer("start using invalid auth code") { host =>
    val response = basicRequest.get(uri"$host/start?code=invalid").send()
    assert(response.code == StatusCode.Unauthorized)
  }

  // Not implemented
  testUsingServer("shutdown".ignore) { host =>
    val response = basicRequest.get(uri"$host/shutdown").send()
    assert(response.code.isSuccess)
    assert(response.history.exists(_.code == StatusCode(301)))
  }

  testUsingServer("run - unauthorized") { host =>
    val response = basicRequest.get(uri"$host/run").send()
    assert(response.code.isClientError)
    assert(response.body.left.exists(_.contains("You might be using outdated session")))
  }

  testUsingServer("run - authorized".ignore) { host =>
    val response = basicRequest.get(uri"$host/run").header(Header.authorization("Bearer", "S0meT0k3n")).send()
    assert(response.is200) // TODO: validation of tokens
  }

  testUsingServer("login") { host =>
    val respone = quickRequest.get(uri"$host/login").send()
    assert(respone.is200)
    assert(respone.body.contains("<title>Logowanie - Spotify</title>"))
  }

  private def base64Encode(v: String): String = Base64.getEncoder.encodeToString(v.getBytes(StandardCharsets.UTF_8))
  private def testSubmit(options: TestOptions)(guess: String, answer: String) = testUsingServer(options) { host =>
    val isCorrect = guess == answer
    val response = quickRequest
      .post(uri"$host/submit")
      .body("guess" -> guess, "encodedAnswear" -> base64Encode(answer))
      .send()
    assert(response.is200)
    if isCorrect
    then assert(response.body.contains("Correct!"))
    else assert(response.body.contains("Sorry, that's something else!"))
  }

  testSubmit("submit - wrong answear")(guess = "Monad czy Monoid w kategori endofuktorów", answer = "Simple Scala")
  testSubmit("submit - wrong answear, close match")(guess = "Simple Java", answer = "Simple Scala")
  testSubmit("submit - correct answear")(guess = "Simple Scala", answer = "Simple Scala")
}

class SpotifyTest extends munit.FunSuite {
  test("clientCredentials"):
    assert(Spotify.Auth.clientCredentials().isDefined)

  test("authorizationCode"):
    assert(Spotify.Auth.authorizationCode("aaa").isEmpty)

  test("loginUrl") {
    val queryParams = Seq(
      s"client_id=${Spotify.ClientId}",
      "response_type=code",
      "redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fstart",
      "scope=user-read-playback-state+user-modify-playback-state"
    ).mkString("&")
    assertEquals(
      Spotify.Auth.AuthorizationUrl,
      s"${Spotify.SpotifyAuthUrl}/authorize?$queryParams"
    )
  }
}
