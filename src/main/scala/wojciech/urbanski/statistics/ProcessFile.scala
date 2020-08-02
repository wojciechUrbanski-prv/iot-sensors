package wojciech.urbanski.statistics

import java.nio.file.Path

import cats.Monad
import cats.effect.{Blocker, ContextShift, Resource, Sync}
import cats.syntax.apply._
import fs2.io.file
import fs2.{text, Stream}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import wojciech.urbanski.sensordata.{SensorData, SensorFileData}

class ProcessFile[F[_]: ContextShift: Sync: Monad] {

  private val ioBlocker: Resource[F, Blocker] = Blocker[F]

  implicit val unsafeLogger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

  def readFromFilesAndGatherStatistics(files: List[Path]): F[OverallStatistics] = {
    readFiles(files).compile.fold(OverallStatistics.empty)(gatherData)
  }

  private def fileToSensorsData(line: String): Option[SensorData] = {
    SensorData.fromFileData(SensorFileData(line))
  }

  private def readFiles(filesPaths: List[Path]): Stream[F, (Option[SensorData], Path)] = {
    Stream
      .resource(ioBlocker)
      .flatMap(blocker => {
        for {
          filePath <- Stream.emits(filesPaths).evalTap(path => Logger[F].info(s"Reading ${path.toString}"))
          lines <- file
                     .readAll[F](filePath, blocker, 4096)
                     .through(text.utf8Decode)
                     .through(text.lines)
                     .zipWithIndex
                     .onEach(1000, index => Logger[F].debug(s"Processed $index elements from $filePath"))
                     .onFinalize(Logger[F].info(s"Successfully processed $filePath"))
        } yield (fileToSensorsData(lines), filePath)
      })
  }

  private def gatherData(acc: OverallStatistics, maybeDataWithPath: (Option[SensorData], Path)) =
    maybeDataWithPath match {
      case (Some(sensorData), path) => acc.addNewSensorData(path.toString, sensorData)
      case (None, path)             => acc.updateFiles(path.toString)
    }

  implicit class StreamWithIndexOps[O](stream: Stream[F, (O, Long)]) {

    def onEach[A](elementNumber: Long, ops: Long => F[A]): Stream[F, O] =
      stream.evalMap {
        case (res, index) if index % elementNumber == 0 && index != 0 => ops(index) *> Monad[F].point(res)
        case (res, _) => Monad[F].point(res)
      }
  }
}
