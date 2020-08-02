package wojciech.urbanski

import java.nio.file.Path

import cats.Show
import cats.effect.{ExitCode, IO}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import wojciech.urbanski.statistics.{OverallStatistics, ProcessFiles}

object Cli
    extends CommandIOApp(
      name = "iot-statistics",
      header = "iot-statistics",
      version = "0.0.1"
    ) {

  override def main: Opts[IO[ExitCode]] = {
    pathParam.map { path =>
      for {
        logger <- Slf4jLogger.create[IO]
        res    <- new ProcessFiles[IO]().readFromFilesAndGatherStatistics(path)
        _      <- logger.info("\n" + Show[OverallStatistics].show(res))
      } yield ExitCode.Success
    }
  }

  val pathParam: Opts[Path] = Opts.option[Path]("path", "Path to the input files.")
}
