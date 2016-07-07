package com.machinomy.consensus

case class Production(volume: Double, cost: Double = 0)

object Production {
  implicit val numeric = new Numeric[Production] {
    override def plus(x: Production, y: Production): Production = Production(x.volume + y.volume, math.max(x.cost, y.cost))

    override def toDouble(x: Production): Double = x.volume

    override def toFloat(x: Production): Float = x.volume.toFloat

    override def toInt(x: Production): Int = x.volume.toInt

    override def negate(x: Production): Production = Production(-x.volume, x.cost)

    override def fromInt(x: Int): Production = Production(x.toDouble, 0)

    override def toLong(x: Production): Long = x.volume.toLong

    override def times(x: Production, y: Production): Production = Production(x.volume * y.volume, x.cost)

    override def minus(x: Production, y: Production): Production = Production(x.volume - y.volume, x.cost)

    override def compare(x: Production, y: Production): Int = implicitly[Numeric[Double]].compare(x.volume, y.volume)
  }
}
