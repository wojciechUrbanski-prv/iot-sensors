package wojciech.urbanski

import java.io.File
import java.nio.file.{Path, Paths}

import cats.Show
import cats.effect.{ExitCode, IO}
import com.monovore.decline.Opts
import com.monovore.decline.effect.CommandIOApp
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
        files <- IO(new File(path.toString).listFiles().filter(_.isFile).filter(_.getName.endsWith(".csv")).map(file => Paths.get(file.getPath)).toList)
        _     <- new ProcessFile[IO]().readFromFilesAndGatherStatistics(files).flatMap(r => IO(println(Show[OverallStatistics].show(r))))
      } yield ExitCode.Success
    }
  }

  val pathParam: Opts[Path] = Opts.option[Path]("path", "Path to the input files.")
}
