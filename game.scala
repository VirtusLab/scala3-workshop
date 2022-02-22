//using lib "com.lihaoyi:cask_3:0.8.0"
//using lib "com.lihaoyi:requests_3:0.7.0"
//using lib "com.lihaoyi:scalatags_sjs1_3:0.11.0"
//using lib "com.lihaoyi:os-lib_3:0.8.0"
//using lib "com.lihaoyi:upickle_3:1.4.4"
//using lib "com.github.vickumar1981:stringdistance_2.13:1.2.6"

// using scala 3.0.0

package workshop

import cask.*
import requests.{Response => rResponse, head=>rHead, *}
import scalatags.Text.all.*
import os.*
import upickle.default.{ReadWriter => RW, macroRW, read, *}
// import upickle.default.ReadWriter.join

import com.github.vickumar1981.stringdistance.LevenshteinDistance

import java.util.Base64
import java.nio.charset.StandardCharsets

import scala.concurrent.*
import scala.util.Random

object CaskHttpServer extends cask.MainRoutes{

    var continue = true
    println("Logging client into Spotify")
    val auth = Spotify.clientCredentials()
    println("Getting playlist...")
    val playlist: PlaylistRef = Spotify.playlists(auth, "propensive").head
    println("Getting tracks from playlist...")
    val items: List[Entry] = Spotify.playlistItems(auth, playlist)
    println(s"Fetched ${items.size} tracks")
    println("Starting web server on port 8080")

    var authCode: Option[RequestAuth] = None

    @get("/")
    def hello() = {  
      Template(
          "Start",
          h1("Welcome"),
          div("Please", a(href:="/login")("log in"), " to play.")
      )
      // println(temp)
      // cask.Response(
      //   "Hello"
      // )
    }

    @get("/start")
    def start(code: String) = {
        val auth = Spotify.authorizationCode(code)
        authCode = Some(auth)
        Redirect("/run")
    }

    @get("/shutdown")
    def shutdown() = {
        continue = false
        Redirect("/")
    }

    // @get("/run")
    // def run() = {
    //     if authCode.isEmpty then Redirect("/login")
    //     else
    //         val chosen = items(Random.nextInt(items.size))
    //         val answer = chosen.track.name.text
    //         val auth = authCode.get

    //         try
    //             Spotify.play(auth, playlist, items.indexOf(chosen))

    //             Future {              // check Future
    //                 Thread.sleep(7500)
    //                 Spotify.pause(auth)
    //             }

    //             Response(
    //                 Template(
    //                     "Guess!",
    //                     h1("Guess the song!"),
    //                     div("Clue: ${chosen.track.name.initials}"),
    //                     form(action := "/submit", method := "post")(
    //                         input(
    //                             `type` := "text", 
    //                             name := "anwer", 
    //                             placeholder := Base64.getEncoder.encodeToString(answer.getBytes(StandardCharsets.UTF_8)), 
    //                             width := "0%"),
    //                         input(
    //                             `type` := "text", 
    //                             name := "guess", 
    //                             width := "80%"),
    //                         input(
    //                             `type` := "submit", 
    //                             value := "Guess", 
    //                             width := "20%")
    //                     )
    //                 )
    //             )

    //         catch
    //             case err: Error =>
    //                 Response(
    //                     Template("Error!", div("An unknown error has occurred"))
    //                 )
    // }

    // @get("/login")
    // def login() = {
    //     Redirect(Spotify.loginUrl)
    // }

    // @get("/submit")
    // def submit(answer: String, guess: String) = {
    //     val name = new String(Base64.getDecoder.decode(answer), StandardCharsets.UTF_8)
    //     Response(
    //         Template(
    //             "Your results...",
    //             h1(
    //                 // if lev(name.toLowerCase(), guess.toLowerCase()) < 4 then "Well done!"
    //                 if LevenshteinDistance.distance(name.toLowerCase(), guess.toLowerCase()) < 4 then "Well done!"
    //                 else "Whoops..."
    //             ),
    //             div(
    //                 s"The correct answer was ${name}. ", 
    //                 a(href:="/run")("Another go?")
    //             )
    //         )
    //     )
    // }

    initialize()
}

object Template:
  def styles: String = os.read(os.pwd/"styles.css")
  def apply(title: String, content: scalatags.generic.Modifier[scalatags.text.Builder]*)  =
    // "<!DOCTYPE html>" + html(head(link(styles), tag("title")(title)), body(content: _*))
  html(head(link(styles), tag("title")(title)), body(content: _*))

// case class Name(text: String) extends AnyVal
type Name = String
// object Name {
//   def apply(name: String): Name = name
// }

case class Entry(track: Track)
case class Track(album: Album, name: Name)
case class Album(name: Name, artists: List[Artist])
case class Artist(name: Name)
case class Playlist(items: List[Entry])
case class PlaylistRef(uri: String, name: Name, id: String)

object Spotify:
  private val ClientId = "7154131346a640ee82dbd02ccfb72a50"
  private val ClientSecret = "f81e25c96ee44af7967179bb3c6998e2"
  private val clientAuth = RequestAuth.Basic(ClientId, ClientSecret)
  private val RedirectUrl = "http://introsgame.cc:8080/start"
  private val urlEncoded = "application/x-www-form-urlencoded"

  def clientCredentials(): RequestAuth =
    val r = requests.post(
      "https://accounts.spotify.com/api/token", 
      headers = Map("Content_Type" -> urlEncoded),
      params = Map("grant_type" -> "client_credentials"),
      auth = clientAuth
    )

    val rjson = ujson.read(r.text())
    RequestAuth.Bearer(read(rjson("access_token")))

  def authorizationCode(code: String): RequestAuth = 
    val r = requests.post(
        "https://accounts.spotify.com/api/token",
        headers = Map("Content_Type" -> urlEncoded),
        params = Map(
        "grant_type" -> "authorization_code",
        "code" -> code,
        "redirect_uri" -> RedirectUrl
        ),
        auth = clientAuth
    )
    RequestAuth.Bearer(r.headers("access_token").mkString(","))


  def loginUrl: String =
      "https://accounts.spotify.com/authorize" // TO-DO

  def playlists(auth: RequestAuth, user: String): List[PlaylistRef] =
    val r = requests.get(
        s"https://api.spotify.com/v1/users/$user/playlists",
        auth = auth
    )

    val rjson = ujson.read(r.text())
    given playlistRefRW:upickle.default.ReadWriter[PlaylistRef] = upickle.default.macroRW[PlaylistRef]
    read[List[PlaylistRef]](rjson("items"))

  def playlistItems(auth: RequestAuth, playlist: PlaylistRef): List[Entry] =
    val r = requests.get(
        s"https://api.spotify.com/v1/playlists/${playlist.id}/tracks",
        auth = auth
    )

    val rjson = ujson.read(r.text())

    given artistR:upickle.default.Reader[Artist] = upickle.default.macroR[Artist]
    given albumR:upickle.default.Reader[Album] = upickle.default.macroR[Album]
    given trackR:upickle.default.Reader[Track] = upickle.default.macroR[Track]
    given entryR:upickle.default.Reader[Entry] = upickle.default.macroR[Entry]
    upickle.default.read[List[Entry]](rjson("items"))

  def play(auth: RequestAuth, playlist: PlaylistRef, offset: Int)=//: HttpStatus =
    case class Offset(position: Int)
    case class Body(context_uri: String, offset: Offset, position_ms: Int)
    implicit def offsetR:upickle.default.ReadWriter[Offset] = upickle.default.macroRW[Offset]
    implicit val rw: upickle.default.ReadWriter[Body] = upickle.default.macroRW[Body]

    val body = Body(playlist.uri, Offset(offset), 0)
    
    val r = requests.put(
        "https://api.spotify.com/v1/me/player/play",
        auth = auth,
        // body = upickle.default.writable(body)
    ) // TO-DO

  def pause(auth: RequestAuth): Unit = 
      val r = requests.put(
        "https://api.spotify.com/v1/me/player/pause",
        auth = auth
    )
