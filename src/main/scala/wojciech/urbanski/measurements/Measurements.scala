package wojciech.urbanski.measurements

case class Measurements(value: Long) extends AnyVal {
  def inc(): Measurements = Measurements(value + 1)
}

object Measurements {

  val zero: Measurements = Measurements(0)
  val one: Measurements  = Measurements(1)

}
