package wojciech.urbanski.statistics

import java.nio.file.Path

import cats.effect.{Blocker, ContextShift, Resource, Sync}
import fs2.{io, text, Stream}
import wojciech.urbanski.sensordata.{SensorData, SensorFileData}

class ProcessFile[F[_]: ContextShift: Sync] {

  private val ioBlocker: Resource[F, Blocker] = Blocker[F]

  def readFromFilesAndGatherStatistics(files: List[Path]): F[OverallStatistics] =
    processFile(files).compile
      .fold(OverallStatistics.empty)(gatherData)

  private def fileToSensorsData(line: String): Option[SensorData] = {
    SensorData.fromFileData(SensorFileData(line))
  }

  private def processFile(filesPaths: List[Path]): Stream[F, (Option[SensorData], Path)] = {
    Stream
      .resource(ioBlocker)
      .flatMap(blocker => {
        for {
          filePath <- Stream.emits(filesPaths)
          lines    <- io.file.readAll[F](filePath, blocker, 4096).through(text.utf8Decode).through(text.lines)
        } yield (fileToSensorsData(lines), filePath)
      })
  }

  private def gatherData(acc: OverallStatistics, maybeDataWithPath: (Option[SensorData], Path)) =
    maybeDataWithPath match {
      case (Some(sensorData), path) => acc.addNewSensorData(path.toString, sensorData)
      case (None, path)             => acc.updateFiles(path.toString)
    }
}
