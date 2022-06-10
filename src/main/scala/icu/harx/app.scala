package icu.harx

import cats.*
import cats.effect.*
import cats.implicits.*
import org.http4s.circe.*
import org.http4s.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.dsl.*
import org.http4s.dsl.impl.*
import org.http4s.headers.*
import org.http4s.implicits.*
import org.http4s.server.*
import org.http4s.server.blaze.BlazeServerBuilder
import org.typelevel.ci.CIString

import java.time.Year
import java.util.UUID
import scala.collection.mutable
import scala.util.Try
import scala.concurrent.ExecutionContext

object app extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    import routes.*
    import scala.concurrent.ExecutionContext.global

    BlazeServerBuilder[IO](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(apis)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)

  object types:

    type Actor = String

    case class Movie(
        id: String,
        title: String,
        year: Year,
        actors: List[String],
        director: String
    )

    case class Director(firstName: String, lastName: String):
      override def toString(): String = firstName + " " + lastName

    object DirectorQueryParamDecoderMatcher extends QueryParamDecoderMatcher[String]("director")

    object DirectorVar:
      def unapply(str: String): Option[Director] =
        if str.nonEmpty && str.matches(""".* .*""") then
          Try {
            val splited = str.split(' ')
            Director(splited(0), splited(1))
          }.toOption
        else None

    extension (year: Int) def year: Year = Year.of(year)

    given QueryParamDecoder[Year] = QueryParamDecoder[Int].emap { year =>
      Try(Year.of(year)).toEither
        .leftMap { throwable =>
          ParseFailure(throwable.toString, throwable.getMessage)
        }
    }

    object YearQueryParamDecoderMatcher extends OptionalValidatingQueryParamDecoderMatcher[String]("year")

  object datas:

    import types.*
    import scala.language.postfixOps

    val snjl: Movie = Movie(
      "6bcbca1e-efd3-411d-9f7c-14b872444fce",
      "Zack Snyder's Justice League",
      2021 year,
      List(
        "Henry Cavill",
        "Gal Godot",
        "Ezra Miller",
        "Ben Affleck",
        "Ray Fisher",
        "Jason Momoa"
      ),
      "Zack Snyder"
    )

    val movies: Map[String, Movie]              = Map(snjl.id -> snjl)
    val directors: mutable.Map[Actor, Director] = mutable.Map("Zack Snyder" -> Director("Zack", "Snyder"))

  object funcs:

    import types.*
    import datas.*

    def findMoviesByDirector(director: String): List[Movie] = movies.values.filter(_.director == director).toList
    def findMovieById(movieId: UUID): Option[Movie]         = movies.get(movieId.toString)

  object routes:

    import types.{DirectorQueryParamDecoderMatcher as DirectorMatcher, YearQueryParamDecoderMatcher as YearMatcher, *}
    import datas.*
    import funcs.*

    def movieRoutes[F[*]: Monad]: HttpRoutes[F] =
      val dsl = Http4sDsl[F]
      import dsl.*

      HttpRoutes.of[F] {
        case GET -> Root / "movies" :? DirectorMatcher(director) +& YearMatcher(maybeYear) =>
          val movies = findMoviesByDirector(director)
          maybeYear match
            case Some(year) =>
              year.fold(
                _ => BadRequest("The given year is not valid"),
                yearStr => Ok(movies.filter(_.year.getValue equals yearStr.toInt).asJson)
              )
            case None => Ok(movies.asJson)

        case GET -> Root / "movies" / UUIDVar(movieId) / "actors" =>
          findMovieById(movieId).map(_.actors) match
            case Some(actors) => Ok(actors.asJson)
            case None         => NotFound(s"No movie with id $movieId found")
      }

    def directorRoutes[F[*]: Concurrent]: HttpRoutes[F] =
      val dsl = Http4sDsl[F]
      import dsl.*

      given EntityDecoder[F, Director] = jsonOf[F, Director]

      HttpRoutes.of[F] {
        case GET -> Root / "directors" / DirectorVar(director) =>
          directors.get(director.toString) match
            case Some(dir) => Ok(dir.asJson, Header.Raw(CIString("My-Custom-Header"), "value"))
            case None      => NotFound(s"No director called $director found")
        case req @ POST -> Root / "directors" =>
          for
            director <- req.as[Director]
            _ = directors.put(director.toString, director)
            res <- Ok
              .headers(`Content-Encoding`(ContentCoding.gzip))
              .map(_.addCookie(ResponseCookie("My-Cookie", "value")))
          yield res
      }

    def allRoutes[F[*]: Concurrent]: HttpRoutes[F]   = movieRoutes[F] <+> directorRoutes[F]
    def allRoutesTotal[F[*]: Concurrent]: HttpApp[F] = allRoutes.orNotFound

    val apis = Router(
      "/api"         -> movieRoutes[IO],
      "/api/private" -> directorRoutes[IO]
    ).orNotFound
