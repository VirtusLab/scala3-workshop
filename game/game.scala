//> using toolkit default
/*
Toolkit provides us the set of easy to use libraries
https://docs.scala-lang.org/toolkit/introduction.html
/> using dep com.lihaoyi::upickle:3.3.0 // JSON serialization - https://com-lihaoyi.github.io/upickle/
/> using dep com.lihaoyi::os-lib:0.10.0 // IO and processess  - htps://github.com/com-lihaoyi/os-lib
/> using dep com.softwaremill.sttp.client4::core:4.0.0-M1 // HTTP client - https://sttp.softwaremill.com/en/stable/
/> using dep org.scalameta::munit::1.0.0-M10 // Testing framework https://scalameta.org/munit/
 */
//> using dep com.lihaoyi::cask:0.9.2       // Simple HTTP server, inspired by Pythons Flask - https://com-lihaoyi.github.io/cask/
//> using dep com.lihaoyi::scalatags:0.13.1 // Typed HTML construction - https://com-lihaoyi.github.io/scalatags/
//> using dep com.github.vickumar1981:stringdistance_2.13:1.2.7 // String fuzzy search - https://github.com/vickumar1981/stringdistance
//> using resourceDir "./styles/"

import sttp.client4.quick.RichRequest
import sttp.client4.*

import cask.*
import scalatags.Text.all.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
// Import read from package ujson using alias name 'parse'
import ujson.{read => parse}
import upickle.default.*
import com.github.vickumar1981.stringdistance.LevenshteinDistance
import sttp.model.Header
import sttp.model.MediaType
import java.util.Base64
import scala.concurrent.Future
import scala.concurrent.duration.*
import sttp.model.StatusCode
import sttp.model.HeaderNames
import cask.model.Status.Unauthorized

object HttpServerApp
    extends CaskHttpServer(
      repo = Spotify.Auth
        .clientCredentials()
        .map(Spotify.Repository()(using _))
        .getOrElse(sys.error("Incorrect client credentials"))
    )

class CaskHttpServer(repo: Spotify.Repository) extends cask.MainRoutes {
  import Spotify._
  val playlist = repo.listUserPlaylists(Spotify.UserIdForPlaylists).head
  val tracks = repo.listPlaylist(playlist)

  // Player to control device, needs auth-code received after login
  private var player: Option[Spotify.Player] = None

  @get("/")
  def entrypoint() =
    // Entry point - show link to login
    // Spotify should redirect to /start in the response
    // <div> click <a href=LoginUrl> here </a> to login </div>
    Template("Guess the song!") {
      div(
        "Please click ",
        a(href := Spotify.Auth.AuthorizeUrl)("here"),
        " to login."
      )
    }

  @get("/start")
  def start(code: String) =
    // Get the authorization code from the Spotify
    // Create an instance of Player and redirect to run
    // or show information about failure and try to login again
    Spotify.Auth
      .authorizationCode(code)
      .map { implicit auth =>
        player = Some(Player())
        val redirect = Redirect("/run")
        redirect.copy(headers = redirect.headers ++ Seq(HeaderNames.Authorization -> auth))
      }
      .getOrElse {
        cask.Response(
          statusCode = StatusCode.Unauthorized.code,
          data = Template("Failed to login")(
            div(
              "Failed to login to Spotify using, please try again ",
              a(href := Spotify.Auth.AuthorizeUrl)("here"),
              " to login."
            )
          ).render
        )
      }

  @get("/run")
  def run() = player match {
    case None =>
      cask.Response(
        Template("Outdated session")(
          div(
            "You might be using outdated session, visit ",
            b(a(href := "/start", "to restart the game"))
          )
        ),
        statusCode = StatusCode.Unauthorized.code
      )
    case Some(player) =>
      // Check if authenticated, if not show link to /login
      // <div> Please try to <a href=loginUrl> login in </a> again to play
      // else:
      // choose random song from playlists
      val songIndex = scala.util.Random.nextInt(tracks.size)
      val song = tracks(songIndex)
      lazy val clue = {
        val splitWords: String => List[String] =
          _.split(' ').filterNot(_.isBlank).toList
        def firstAndLastLetters(sentence: String) =
          splitWords(sentence)
            .map(word =>
              if word.size == 2 then word
              else if word.size > 2 then s"${word.head}...${word.last}"
              else word.head
            )
        def initialLetters(sentence: String) =
          splitWords(sentence).flatMap(_.headOption)
        val songName = firstAndLastLetters(song.name).mkString(" ")
        val authors = song.album.artists
          .map(_.name)
          .map(initialLetters(_).mkString(""))
          .mkString(" and ")
        s"$songName by ${authors}"
      }

      // Send request to Spotify to start playing the song on device
      player.play(playlist, songIndex) match {
        case Player.State.NoActiveDevice =>
          cask.Response(
            Template("No active device")(
              div(
                "It looks like none of your Spotify clients is currently active.",
                "Try to use your Spotify client and ",
                b(a(href := "/run", "try again"))
              )
            )
          )

        case Player.State.Ok =>
          // Schedule a request that would stop playing song after some delay
          Future {
            Thread.sleep(8.seconds.toMillis)
            player.pause()
          }

          // Show form where user can insert their guess, it should contain:
          // * some clue about the cong
          // * should send the reuslt to /submit using post and encoding type 'application/x-www-form-urlencoded'
          /* <div> Clue ... </div>
           * <form action=/submit method=post, enctype=UrlEncoded, autocomplete=fales>
           *  <input type=hidden name=answer value=CorrectAnswers, widhth=0%>
           *  <input type=text, name=guess/>
           *  <input type=submit value=Guess/>
           * </form>
           */

          cask.Response(
            Template("Guess!")(
              h1("Your turn, guess the song!"),
              div(s"Clue: $clue"),
              form(
                action := "/submit",
                method := "post",
                enctype := MediaType.ApplicationXWwwFormUrlencoded.toString,
                autocomplete := false
              )(
                input(
                  `type` := "hidden",
                  name := "encodedAnswer",
                  value := new String(
                    Base64.getEncoder.encode(song.name.getBytes)
                  ),
                  width := "0%"
                ),
                input(`type` := "text", name := "guess", width := "80%"),
                input(`type` := "submit", value := "Guess", width := "20%")
              )
            )
          )
      }
  }

  @get("/login")
  def login() = {
    // Redirect to Spotify logining using OAuth
    Redirect(Spotify.Auth.AuthorizeUrl)
  }

  @cask.postForm("/submit")
  def submit(encodedAnswear: String, guess: String) = {
    val answear = String(Base64.getDecoder.decode(encodedAnswear))
    // Check the guess and correct answer
    // Show message wheter message is correct or not
    // Show a link to alowing to retry with different song
    /*
     * <h1>Result</h1>
     * <div>
     *   Correct answer was ...
     *   <a href=/run> Click to try again </a>
     * </div>
     */
    val matchIndex = LevenshteinDistance.distance(answear, guess)
    val isCorrect = matchIndex <= 2
    Template("Result") {
      div(
        h1(if isCorrect then "Correct!" else "Sorry, that's something else!"),
        div(
          p(s"Correct answear: ${answear}"),
          p(s"Your answer: ${guess}")
        ),
        div(a(href := "/run", "Click here to try again"))
      )
    }
  }

  println("Starting web server on port 8080...")
  initialize()
}

object Template:
  // A common template for all the responses
  import scalatags.*
  import scalatags.generic.Modifier
  import scalatags.text.Builder
  def apply(title: String)(content: Modifier[Builder]*) =
    doctype("html"):
      html(
        styles,
        head(tag("title")(title)),
        body(content*)
      )

  private lazy val styles =
    tag("style")(
      os.read(os.resource() / "styles.css")
    )
end Template

object Spotify:
  final val ClientId = "18500f030b0f414890a38c16e7391116"
  private final val ClientSecret = "d015e1ed6d3c4a1082b9fd29b35688e5"
  private final val RedirectUrl = "http://localhost:8080/start"
  final val SpotifyApi = "https://api.spotify.com/v1"
  final val SpotifyAuthUrl = "https://accounts.spotify.com"
  final val UserIdForPlaylists = "spotify"

  object Auth {
    final lazy val AuthorizeUrl: String =
      // https://developer.spotify.com/documentation/web-api/quick-start/ #Call the Spotify Accounts Service
      // https://developer.spotify.com/documentation/general/guides/authorization/code-flow/
      import java.nio.charset.StandardCharsets.UTF_8
      import java.net.URLEncoder.encode
      val scope = "user-read-playback-state user-modify-playback-state"
      val params = Map(
        "client_id" -> ClientId,
        "response_type" -> "code",
        "redirect_uri" -> RedirectUrl,
        "scope" -> scope
      ).map((key, value) => s"$key=${encode(value, UTF_8)}")
        .mkString("&")
      s"$SpotifyAuthUrl/authorize?$params"

    opaque type BearerToken <: String = String
    object BearerToken:
      def apply(v: String): BearerToken = v

    def clientCredentials(): Option[BearerToken] =
      // Get access_token using client-credentials
      // https://developer.spotify.com/documentation/general/guides/authorization/client-credentials/
      // read access_token from response
      val response = quickRequest
        .post(uri"$SpotifyAuthUrl/api/token".addParam("grant_type", "client_credentials"))
        .header(Header.contentType(MediaType.ApplicationXWwwFormUrlencoded))
        .auth
        .basic(ClientId, ClientSecret)
        .send()

      Option
        .when(response.isSuccess):
          parse(response.body)("access_token").str
        .orElse {
          println(response)
          None
        }

    def authorizationCode(code: String): Option[BearerToken] =
      // Use authorization code to obtain auth code
      // https://developer.spotify.com/documentation/general/guides/authorization/code-flow/
      val result = quickRequest
        .post(
          uri"$SpotifyAuthUrl/api/token".addParams(
            "grant_type" -> "authorization_code",
            "code" -> code,
            "redirect_uri" -> RedirectUrl
          )
        )
        .headers(Header.contentType(MediaType.ApplicationXWwwFormUrlencoded))
        .auth
        .basic(ClientId, ClientSecret)
        .send()

      Option.when(result.isSuccess):
        parse(result.body)("access_token").str
  }

  // Repository of Spotify resources, needs only client-credentials auth
  class Repository()(using authToken: Auth.BearerToken) {
    // https://developer.spotify.com/console/get-playlists/
    // https://api.spotify.com/v1/users/{user_id}/playlists
    def listUserPlaylists(user: String): List[PlaylistRef] = {
      val respone = quickRequest
        .get(uri"${SpotifyApi}/users/${user}/playlists")
        .auth
        .bearer(authToken)
        .send()
      val json = parse(respone.body)
      read[List[PlaylistRef]](json("items"))
    }

    // https://developer.spotify.com/console/get-playlist-tracks/
    // https://api.spotify.com/v1/playlists/{playlist_id}/tracks
    def listPlaylist(playlist: PlaylistRef): List[Track] =
      val response = quickRequest
        .get(uri"${SpotifyApi}/playlists/${playlist.id}/tracks")
        .auth
        .bearer(authToken)
        .send()
      val json = parse(response.body)
      read[List[Track]](json("items").arr.map(_("track")))
  }

  // Device controler, needs auth-code authorization
  object Player {
    enum State:
      case Ok, NoActiveDevice
    case object Unauthorized extends Player()(using Auth.BearerToken(""))
  }
  class Player()(using val authToken: Auth.BearerToken) {
    import Player.*

    // https://developer.spotify.com/console/put-play/
    // https://api.spotify.com/v1/me/player/play
    def play(playlist: PlaylistRef, trackIdx: Int) = {
      val response = quickRequest
        .put(uri"$SpotifyApi/me/player/play")
        .auth
        .bearer(authToken)
        .body(
          ujson
            .Obj(
              "context_uri" -> playlist.uri,
              "offset" -> ujson.Obj("position" -> trackIdx),
              "position_ms" -> 15
            )
            .toString
        )
        .send()
      lazy val data = parse(response.body)
      if response.isSuccess then State.Ok
      else
        data("error")("reason").strOpt.collect {
          case "NO_ACTIVE_DEVICE" => State.NoActiveDevice
          case "PREMIUM_REQUIRED" => ??? // TODO: Spotify now requires premium to control playback
        }.get
    }
    // https://developer.spotify.com/console/put-pause/)
    // https://api.spotify.com/v1/me/player/pause
    def pause(): Unit =
      quickRequest
        .put(uri"$SpotifyApi/me/player/pause")
        .auth
        .bearer(authToken)
        .send()
  }

case class PlaylistRef(uri: String, name: String, id: String) derives Reader
case class Track(album: Album, name: String) derives Reader
case class Album(name: String, artists: List[Artist]) derives Reader
case class Artist(name: String) derives Reader
