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
import sttp.model.*

import scalatags.Text.all.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
// Import read from package ujson using alias name 'parse'
import ujson.{read => parse}
import upickle.default.*

import com.github.vickumar1981.stringdistance.LevenshteinDistance
import java.util.Base64

object HttpServerApp
    extends CaskHttpServer(
      repo = ???
    )

class CaskHttpServer(repo: Spotify.Repository) extends cask.MainRoutes {
  import cask.*
  // Player to control device, needs auth-code received after login
  def player: Option[Spotify.Player] = None

  @get("/")
  def entrypoint() =
    // Entry point - show link to login
    // Spotify should redirect to /start in the response
    // <div> click <a href=LoginUrl> here </a> to login </div>
    ???
  @get("/start")
  def start(code: String) =
    // Get the authorization code from the Spotify
    // Create an instance of Player and redirect to /run
    // or show information about failure and try to login again
    ???

  @get("/run")
  def run() = {
    // Check if authenticated, if not show link to /login
    // <div> Please try to <a href=loginUrl> login in </a> again to play
    // else:
    // choose random song from playlists
    // Send request to Spotify to start playing the song on device
    // Schedule a request that would stop playing song after some delay
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
    ???
  }

  @get("/login")
  def login() = {
    // Redirect to Spotify logining using OAuth
    Redirect(Spotify.Auth.AuthorizationUrl)
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
    ???
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
  final val ClientId = "SetMeUp"
  private final val ClientSecret = "SetMeUp"
  private final val RedirectUrl = "http://localhost:8080/start"
  final val SpotifyApi = "https://api.spotify.com/v1"
  final val SpotifyAuthUrl = "https://accounts.spotify.com"
  final val UserIdForPlaylists = "spotify"

  object Auth {
    def AuthorizationUrl: String =
      // https://developer.spotify.com/documentation/web-api/quick-start/ #Call the Spotify Accounts Service
      // https://developer.spotify.com/documentation/general/guides/authorization/code-flow/
      val scope = "user-read-playback-state user-modify-playback-state"
      val params = ???
      s"$SpotifyAuthUrl/authorize?$params"

    type BearerToken
    def BearerToken(unsafeToken: String): BearerToken = ???
    def clientCredentials(): Option[BearerToken] =
      // Get access_token using client-credentials
      // https://developer.spotify.com/documentation/general/guides/authorization/client-credentials/
      // read access_token from response
      ???

    def authorizationCode(code: String): Option[BearerToken] =
      // Use authorization code to obtain auth code
      // https://developer.spotify.com/documentation/general/guides/authorization/code-flow/
      ???
  }

  // Repository of Spotify resources, needs only client-credentials auth
  class Repository()(using authToken: Auth.BearerToken) {
    // https://developer.spotify.com/console/get-playlists/
    // https://api.spotify.com/v1/users/{user_id}/playlists
    def listUserPlaylists(user: String): List[PlaylistRef] = ???

    // https://developer.spotify.com/console/get-playlist-tracks/
    // https://api.spotify.com/v1/playlists/{playlist_id}/tracks
    def listPlaylist(playlist: PlaylistRef): List[Track] = ???
  }

  type PlaylistRef
  type Track
  type Album
  type Artist

  // Device controler, needs auth-code authorization
  trait Player {
    // https://developer.spotify.com/console/put-play/
    // https://api.spotify.com/v1/me/player/play
    def play(playlist: PlaylistRef, trackIdx: Int) = ???

    // https://developer.spotify.com/console/put-pause/)
    // https://api.spotify.com/v1/me/player/pause
    def pause(): Unit = ???
  }

