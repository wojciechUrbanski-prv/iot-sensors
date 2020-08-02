package wojciech.urbanski

import java.io.File
import java.nio.file.{Path, Paths}

import cats.Show
import cats.effect.{ExitCode, IO}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import wojciech.urbanski.statistics.{OverallStatistics, ProcessFile}

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
        files  <- IO(new File(path.toString).listFiles().filter(_.isFile).filter(_.getName.endsWith(".csv")).map(file => Paths.get(file.getPath)).toList)
        _ <- if (files.isEmpty) logger.info("Could not find any .csv files in provided location")
             else
               new ProcessFile[IO]()
                 .readFromFilesAndGatherStatistics(files)
                 .flatMap(r => logger.info("\n" + Show[OverallStatistics].show(r)))
      } yield ExitCode.Success
    }
  }

  val pathParam: Opts[Path] = Opts.option[Path]("path", "Path to the input files.")
}
