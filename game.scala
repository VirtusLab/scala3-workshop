// using lib "com.propensive:gastronomy-core:0.3.0"
// using lib "com.propensive:scintillate-server:0.3.0"
// using lib "com.propensive:guillotine-core:0.3.0"
// using lib "com.propensive:kaleidoscope-core:0.3.0"
// using lib "com.propensive:euphemism-core:0.3.0"
// using lib "org.typelevel:jawn-parser_3:1.2.0"
// using lib "org.typelevel:jawn-ast_3:1.2.0"
// using lib "com.propensive:acyclicity-core:0.3.0"
// using lib "com.propensive:jovian-core:0.3.0"
// using lib "com.propensive:honeycomb-core:0.3.0"
// using lib "com.propensive:punctuation-html:0.3.0"
// using lib "com.vladsch.flexmark:flexmark:0.42.12"
// using lib "com.vladsch.flexmark:flexmark-ext-tables:0.42.12"

// using scala 3.1.0

package workshop

import gastronomy.*, gossamer.*, scintillate.*, rudiments.*, gesticulate.*,
guillotine.*, kaleidoscope.*, euphemism.*, escapade.*, jovian.*, eucalyptus.*,
honeycomb.*, punctuation.*

import scala.concurrent.*

given enc: Encoding = encodings.Utf8
given Env = envs.enclosing
given ExecutionContext = ExecutionContext.Implicits.global
given LogFormat[Stdout.type, AnsiString] = LogFormat.timed
given Log = Log(Everything |-> Stdout)

@main
def game(): Unit = try
  var continue = true
  Log.info(t"Logging client into Spotify")
  val auth = Spotify.clientCredentials()
  Log.info(t"Getting playlist...")
  val playlist: PlaylistRef = Spotify.playlists(auth, t"propensive").head
  Log.info(t"Getting tracks from playlist...")
  val items: List[Entry] = Spotify.playlistItems(auth, playlist)
  Log.info(t"Fetched ${items.size} tracks")
  Log.info(t"Starting web server on port 8080")

  val secretKey = SymmetricKey.generate[Aes[256]]()

  var authCode: Option[Auth] = None

  // Define some request parameters
  val CodeParam = RequestParam[Text](t"code")
  val GuessParam = RequestParam[Text](t"guess")
  val AnswerParam = RequestParam[Text](t"answer")

  // Start web server
  HttpServer(8080)
    .listen {
      request match
        case UrlPath("/") =>
          Response(
            Template(
              t"Start",
              H1(t"Welcome"),
              Div(md"Please [log in](/login) to play.".html)
            )
          )

        case UrlPath("/start") & CodeParam(code) =>
          val auth = Spotify.authorizationCode(code)
          authCode = Some(auth)
          Response(Redirect(t"/run"))

        case UrlPath("/shutdown") =>
          continue = false
          Response(Redirect(t"/"))

        case UrlPath("/run") =>
          if authCode.isEmpty then Response(Redirect(t"/login"))
          else
            val chosen = items.random
            val answer = chosen.track.name.text
            val auth = authCode.get

            try
              Spotify.play(auth, playlist, items.indexOf(chosen))

              Future {
                Thread.sleep(7500)
                Spotify.pause(auth)
              }

              Response(
                Template(
                  t"Guess!",
                  H1(t"Guess the song!"),
                  Div(t"Clue: ${chosen.track.name.initials}"),
                  Form(
                    method = Method.Post,
                    autocomplete = false,
                    action = t"/submit",
                    enctype = media"application/x-www-form-urlencoded"
                  )(
                    Input(name = GuessParam),
                    Input(
                      name = AnswerParam,
                      htype = HType.Hidden,
                      value = secretKey.encrypt(answer).encode[Base64]
                    ),
                    Input(htype = HType.Submit, value = t"Guess")
                  )
                )
              )

            catch
              case err: HttpError =>
                err match
                  case HttpError(HttpStatus.NotFound, content) =>
                    if content
                        .as[Json]
                        .error
                        .reason
                        .as[Text] == t"NO_ACTIVE_DEVICE"
                    then
                      Response(
                        Template(
                          t"Error!",
                          Div(t"Please open Spotify on your phone!")
                        )
                      )
                    else
                      Response(
                        Template(
                          t"Error!",
                          Div(t"An unknown error has occurred")
                        )
                      )
                  case _ =>
                    Response(
                      Template(t"Error!", Div(t"An unknown error has occurred"))
                    )

        case UrlPath("/login") =>
          Response(Redirect(Spotify.loginUrl))

        case UrlPath("/submit") & AnswerParam(answer) & GuessParam(guess) =>
          val name = secretKey.decrypt[Text](answer.decode[Base64])
          Response(
            Template(
              t"Your results...",
              H1(
                if name.lower.lev(guess.lower) < 4 then t"Well done!"
                else t"Whoops..."
              ),
              Div(
                md"The correct answer was _${name}_. [Another go](/run)?".html
              )
            )
          )

        case _ =>
          Response(NotFound(Template(t"Not found", H1(t"Not found"))))
    }
    .await()

catch
  case err: Exception =>
    println(err)
    err.printStackTrace()

object Template:
  def styles: Text = (Unix.Pwd / t"styles.css").file.read[Text](20.kb)
  def apply(title: Text, content: Html[Flow]*): HtmlDoc =
    HtmlDoc(Html(Head(Style(styles), Title(title)), Body(content*)))

given Json.Reader[Name] = summon[Json.Reader[Text]].map(Name(_))

case class Name(text: Text):
  def initials: Text = text.cut(t" ").map(_.take(1)).join(t"", t". ", t".")

case class Entry(track: Track)
case class Track(album: Album, name: Name)
case class Album(name: Name, artists: List[Artist])
case class Artist(name: Name)
case class Playlist(items: List[Entry])
case class PlaylistRef(uri: Text, name: Name, id: Text)

object Spotify:
  private val ClientId = t"7154131346a640ee82dbd02ccfb72a50"
  private val ClientSecret = t"f81e25c96ee44af7967179bb3c6998e2"
  private val clientAuth = Auth.Basic(ClientId, ClientSecret)
  private val RedirectUrl = t"http://introsgame.cc:8080/start"
  private val urlEncoded = media"application/x-www-form-urlencoded"

  def clientCredentials(): Auth =
    val url = uri"https://accounts.spotify.com/api/token".query(grant_type =
      t"client_credentials"
    )
    val json = url
      .post(clientAuth(), RequestHeader.ContentType(urlEncoded.show))(())
      .as[Json]
    Auth.Bearer(json.access_token.as[Text])

  def authorizationCode(code: Text): Auth =
    val url = uri"https://accounts.spotify.com/api/token".query(
      grant_type = t"authorization_code",
      code = code,
      redirect_uri = RedirectUrl
    )

    val json = url
      .post(clientAuth(), RequestHeader.ContentType(urlEncoded.show))(())
      .as[Json]
    Auth.Bearer(json.access_token.as[Text])

  def loginUrl: Uri =
    uri"https://accounts.spotify.com/authorize".query(
      client_id = ClientId,
      response_type = t"code",
      redirect_uri = RedirectUrl,
      scope = t"user-read-playback-state user-modify-playback-state"
    )

  def playlists(auth: Auth, user: Text): List[PlaylistRef] =
    val json =
      uri"https://api.spotify.com/v1/users/$user/playlists".get(auth()).as[Json]
    json.items.as[List[PlaylistRef]]

  def playlistItems(auth: Auth, playlist: PlaylistRef): List[Entry] =
    val json = uri"https://api.spotify.com/v1/playlists/${playlist.id}/tracks"
      .get(auth())
      .as[Json]
    json.items.as[List[Entry]]

  def play(auth: Auth, playlist: PlaylistRef, offset: Int): HttpStatus =
    case class Body(context_uri: Text, offset: Offset, position_ms: Int)
    case class Offset(position: Int)
    val body = Body(playlist.uri, Offset(offset), 0)
    uri"https://api.spotify.com/v1/me/player/play"
      .put(auth())(body.json)
      .as[HttpStatus]

  def pause(auth: Auth): Unit =
    uri"https://api.spotify.com/v1/me/player/pause".put(auth())(())

given realm: Realm = Realm(t"game")
