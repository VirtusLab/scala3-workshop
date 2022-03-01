// using lib "com.lihaoyi::utest:0.7.10"

// using scala "3.0.0"

package workshop.test

import utest._
import workshop.src.*
import requests.RequestAuth

object GameTests extends TestSuite{
    val artist =  Artist("Queen")
    val album = Album("Bohemian Rhapsody (The Original Soundtrack)", List(artist))
    val track = Track(album, "Bohemian Rhapsody")
    val entry = Entry(track)
    val playlist =  Playlist(List(entry))
    val playlistRef = PlaylistRef("3z8h0TU7ReDPLIbEnYhWZb?si=6fba5329b9f44ad9", "Bohemian Rhapsody", "3z8h0TU7ReDPLIbEnYhWZb?si=6fba5329b9f44ad9")

    val tests = Tests{
        test("Spotify.clientCredentials"){
            val auth: RequestAuth = Spotify.clientCredentials()
            auth
        }
        test("Spotify.authorizationCode_invalidCode"){
            try{
                val auth = Spotify.authorizationCode("aaa")
            } catch {case e: requests.RequestFailedException =>
                e
            }
        }
        test("Spotify.loginUrl"){
            val loginUrl = Spotify.loginUrl
            assert(loginUrl=="https://accounts.spotify.com/authorize?client_id=1588c59aabee43ca9f4d30d5695a4a0c&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fstart&scope=user-read-playback-state+user-modify-playback-state")
        }
    }
}
