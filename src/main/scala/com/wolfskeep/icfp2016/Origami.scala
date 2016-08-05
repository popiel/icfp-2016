package com.wolfskeep.icfp2016

import com.wolfskeep.icfp2016._
import scala.annotation.tailrec
import scala.math._

case class Problem(shape: Shape, skeleton: Skeleton)
case class Shape(polygons: Seq[Polygon]) {
  def twiceArea = polygons.map(_.twiceArea).sum
}
case class Polygon(points: Seq[Point]) {
  def twiceArea = {
    val v0 = points.head
    points.tail.sliding(2).map { case Seq(v1, v2) =>
      (v1.x - v0.x) * (v2.y - v0.y) - (v2.x - v0.x) * (v1.y - v0.y)
    }.sum
  }
}
case class Point(x: Ratio, y: Ratio)
object Point {
  implicit def apply[T <% Ratio](pair: (T, T)): Point = Point(pair._1, pair._2)
}
abstract case class Ratio private[Ratio](num: Int, den: Int) {
  require (den != 0)

  def + (that: Ratio) = Ratio(this.num * that.den + that.num * this.den, this.den * that.den)
  def - (that: Ratio) = Ratio(this.num * that.den - that.num * this.den, this.den * that.den)
  def * (that: Ratio) = Ratio(this.num * that.num, this.den * that.den)
  def / (that: Ratio) = Ratio(this.num * that.den, this.den * that.num)

  def copy(num: Int = num, den: Int = den): Ratio = Ratio.apply(num, den)

  override def toString() = if (den == 1) num.toString else s"$num/$den"
}
object Ratio {
  @tailrec def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  implicit def apply(a: Int): Ratio = Ratio(a, 1)
  def apply(num: Int, den: Int): Ratio = {
    val div = gcd(abs(num), abs(den)) * signum(den)
    new Ratio(num / div, den / div){}
  }
}
class RatioConstruction(a: Int) {
  def /| (b: Int) = com.wolfskeep.icfp2016.Ratio(a, b)
}
object RatioConstruction {
  implicit def apply(a: Int) = new RatioConstruction(a)
}

case class Skeleton(segments: Seq[Segment])
case class Segment(a: Point, b: Point)

