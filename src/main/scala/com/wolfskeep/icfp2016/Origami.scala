package com.wolfskeep.icfp2016

import scala.annotation.tailrec
import scala.math._

case class Problem(shape: Shape, skeleton: Skeleton)
case class Skeleton(segments: Seq[Segment])
case class Segment(a: Point, b: Point) {
  def length2 = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)
}
object Segment {
  implicit def apply(pair: (Point, Point)): Segment = Segment(pair._1, pair._2)
}
case class Shape(polygons: Seq[Polygon]) {
  def twiceArea = polygons.map(_.twiceArea).sum
}
case class Polygon(points: Seq[Point]) {
  private def triSize(j: Int) = {
    val (v0, v1, v2) = (points(0), points(j), points(j + 1))
    (v1.x - v0.x) * (v2.y - v0.y) - (v2.x - v0.x) * (v1.y - v0.y)
  }

  def twiceArea = (1 until points.length - 1).map(triSize).sum

  def congruent(that: Polygon): Boolean = {
    this.points.length == that.points.length &&
    Segment(this.points(0) -> this.points(1)).length2 == Segment(that.points(0) -> that.points(1)).length2 &&
    ( (1 until points.length - 1).forall(j => this.triSize(j) == that.triSize(j)) ||
      (1 until points.length - 1).forall(j => this.triSize(j) == -that.triSize(j)))
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

  def unary_+ = this
  def unary_- = Ratio(-num, den)

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

