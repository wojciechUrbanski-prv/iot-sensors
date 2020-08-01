package wojciech.urbanski.measurements

case class Measurements(value: Long) extends AnyVal {
  def inc(): Measurements = Measurements(value + 1)
  def add(secondMeasurements: Measurements): Measurements = Measurements(value + secondMeasurements.value)
  def subtract(secondMeasurements: Measurements): Measurements = Measurements(value - secondMeasurements.value)
}

object Measurements {

  val zero: Measurements = Measurements(0)
  val one: Measurements = Measurements(1)

}