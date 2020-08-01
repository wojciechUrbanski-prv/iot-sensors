package wojciech.urbanski

import java.io.File
import java.nio.file.{Path, Paths}

import cats.Show
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import fs2.{Stream, io, text}
import wojciech.urbanski.sensordata.{SensorData, SensorFileData}
import wojciech.urbanski.statistics.OverallStatistics

object Converter extends IOApp {

  val ioBlocker: Resource[IO, Blocker] = Blocker[IO]

  def fileToSensorsData(line: String): Option[SensorData] = {
    SensorData.fromFileData(SensorFileData(line))
  }

  def run(args: List[String]): IO[ExitCode] = {

    def processFile(filesPaths: List[Path]): Stream[IO, (Option[SensorData], Path)] = {
      Stream.resource(ioBlocker).flatMap(blocker => {
        for {
          filePath <- Stream.emits(filesPaths)
          lines <- io.file.readAll[IO](filePath, blocker, 4096).through(text.utf8Decode).through(text.lines)
        } yield (fileToSensorsData(lines), filePath)
      })
    }

    def gatherData(acc: OverallStatistics, maybeDataWithPath: (Option[SensorData], Path)) = maybeDataWithPath match {
      case (Some(sensorData), path) => acc.addNewSensorData(path.toString, sensorData)
      case (None, path) => acc.updateFiles(path.toString)
    }

    val files = new File( "src/testdata").listFiles().filter(_.isFile).map(file => Paths.get(file.getPath)).toList


    val res = processFile(files)
      .compile
      .fold(OverallStatistics.empty)(gatherData)

    res.flatMap(r => IO(println(Show[OverallStatistics].show(r)))).as(ExitCode.Success)
  }


}