package wojciech.urbanski.statistics

import java.io.File
import java.nio.file.{Path, Paths}

import cats.Monad
import cats.effect.{Blocker, ContextShift, Resource, Sync}
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fs2.io.file
import fs2.{Stream, text}
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.{Logger, SelfAwareStructuredLogger}
import wojciech.urbanski.sensordata.{SensorData, SensorFileData}

class ProcessFiles[F[_]: ContextShift: Sync: Monad] {

  private val ioBlocker: Resource[F, Blocker] = Blocker[F]

  implicit val unsafeLogger: SelfAwareStructuredLogger[F] = Slf4jLogger.getLogger[F]

  def readFromFilesAndGatherStatistics(path: Path): F[OverallStatistics] = {
    for {
      files <- Sync[F].delay(new File(path.toString).listFiles().filter(_.isFile).filter(_.getName.endsWith(".csv")).filter(_.getName != "celsius.csv").map(file => Paths.get(file.getPath)).toList)
      res <- if (files.isEmpty) Logger[F].info("Could not find any .csv files in provided location") *> Monad[F].point(OverallStatistics.empty)
             else readFiles(files).compile.fold(OverallStatistics.empty)(gatherData)
    } yield res

  }

  private def fileToSensorsData(line: String): SensorData = {
    SensorData.fromFileData(SensorFileData(line))
  }

  private def readFiles(filesPaths: List[Path]): Stream[F, (SensorData, Path)] = {
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
                     .onFinalize(Logger[F].info(s"Processed $filePath"))
        } yield (fileToSensorsData(lines), filePath)
      })
  }

  private def gatherData(acc: OverallStatistics, dataWithPath: (SensorData, Path)) =
      acc.addNewSensorData(dataWithPath._2, dataWithPath._1)

  implicit class StreamWithIndexOps[O](stream: Stream[F, (O, Long)]) {

    def onEach[A](elementNumber: Long, ops: Long => F[A]): Stream[F, O] =
      stream.evalMap {
        case (res, index) if index % elementNumber == 0 && index != 0 => ops(index) *> Monad[F].point(res)
        case (res, _) => Monad[F].point(res)
      }
  }
}
