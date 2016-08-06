package com.wolfskeep.icfp2016

import scala.annotation.tailrec
import scala.io._
import scala.math._


case class Problem(shape: Shape, skeleton: Skeleton)
object Problem {
  def parse(text: Iterator[String]): Problem = {
    Problem(Shape.parse(text), Skeleton.parse(text))
  }
  def parse(text: String): Problem = parse(Source.fromString(text).getLines)
  def parse(id: Int): Problem = parse(Source.fromFile("data/problems/"+id+"/spec.txt").getLines)
}
case class Skeleton(segments: Seq[Segment]) {
  lazy val all = (segments ++ segments.map(_.flip)).distinct.sortBy(_.length2)
  lazy val points = all.map(_.a).distinct
  lazy val connecting: Map[Point,Seq[Segment]] = all.groupBy(_.a)
  lazy val rightAngles: Seq[(Segment, Segment)] = {
    for {
      point <- points
      set <- connecting(point).tails
      if set.length >= 2
      s1 = set.head
      s2 <- set.tail
      if s1 right s2
    } yield (s1, s2)
  }.toSeq.sortBy(_._1.length2)
}
object Skeleton {
  def parse(text: Iterator[String]) = {
    val count = text.next.toInt
    Skeleton((1 to count).map{ _ => Segment.parse(text) }.toSeq.sortBy(_.length2))
  }
}
case class Segment(a: Point, b: Point) {
  // require(a != b)

  def flip = Segment(b, a)

  def length2 = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)

  def right(that: Segment) = {
    this.a == that.a && {
      val twiceArea = Polygon(List(this.a, this.b, that.b)).twiceArea
      this.length2 * that.length2 == twiceArea * twiceArea
    }
  }

  def reflect(p: Point): Point = {
    if (a.x == b.x)
      Point(a.x * 2 - p.x, p.y)
    else if (a.y == b.y)
      Point(p.x, a.y * 2 - p.y)
    else {
      val m = (b.y - a.y) / (b.x - a.x)
      val c = b.y - b.x * m
      val d = (p.x + (p.y - c)*m) / (m * m + 1)
      Point(d * 2 - p.x, d * m * 2 - p.y + c * 2)
    }
  }

  def colinear(p: Point) = Polygon(List(a, b, p)).twiceArea.num == 0

  def unit = length2.sqrt.map(l => Segment(a, (b - a) / l + a))
}
object Segment {
  implicit def apply(pair: (Point, Point)): Segment = Segment(pair._1, pair._2)
  def parse(text: Iterator[String]) = {
    val s = text.next
    val points = s.split(" ")
    Segment(Point.parse(points(0)), Point.parse(points(1)))
  }
}
case class Shape(polygons: Seq[Polygon]) {
  def twiceArea = polygons.map(_.twiceArea).sum
}
object Shape {
  def parse(text: Iterator[String]) = {
    val count = text.next.toInt
    Shape((1 to count).map{ _ => Polygon.parse(text) }.toSeq)
  }
}
case class Polygon(points: Seq[Point]) {
  private def triSize(j: Int) = {
    val (v0, v1, v2) = (points(0), points(j), points(j + 1))
    (v1.x - v0.x) * (v2.y - v0.y) - (v2.x - v0.x) * (v1.y - v0.y)
  }

  def twiceArea = if (points.length == 3) triSize(1) else (1 until points.length - 1).map(triSize).sum

  def congruent(that: Polygon): Boolean = {
    this.points.length == that.points.length &&
    Segment(this.points(0) -> this.points(1)).length2 == Segment(that.points(0) -> that.points(1)).length2 &&
    ( (1 until points.length - 1).forall(j => this.triSize(j) == that.triSize(j)) ||
      (1 until points.length - 1).forall(j => this.triSize(j) == -that.triSize(j)))
  }

  def isRight: Boolean = {
    points.length == 3 &&
    Segment(points(0),points(1)).length2 * Segment(points(0), points(2)).length2 == triSize(1) * triSize(1)
  }
}
object Polygon {
  def parse(text: Iterator[String]) = {
    val count = text.next.toInt
    Polygon((1 to count).map{ _ => Point.parse(text) }.toSeq)
  }
}
case class Point(x: Ratio, y: Ratio) {
  override def toString() = s"$x,$y"

  def + (that: Point) = Point(this.x + that.x, this.y + that.y)
  def - (that: Point) = Point(this.x - that.x, this.y - that.y)
  def * (scale: Ratio) = Point(this.x * scale, this.y * scale)
  def / (scale: Ratio) = Point(this.x / scale, this.y / scale)
}
object Point {
  implicit def apply[T <% Ratio](pair: (T, T)): Point = Point(pair._1, pair._2)
  def parse(text: Iterator[String]): Point = parse(text.next)
  def parse(text: String): Point = {
    val nums = text.split(",")
    Point(Ratio.parse(nums(0)), Ratio.parse(nums(1)))
  }
}
abstract case class Ratio private[Ratio](num: BigInt, den: BigInt) extends Ordered[Ratio] {
  require (den != 0)

  def compare(that: Ratio) = (this.num * that.den - that.num * this.den).signum

  def + (that: Ratio) = Ratio(this.num * that.den + that.num * this.den, this.den * that.den)
  def - (that: Ratio) = Ratio(this.num * that.den - that.num * this.den, this.den * that.den)
  def * (that: Ratio) = Ratio(this.num * that.num, this.den * that.den)
  def / (that: Ratio) = Ratio(this.num * that.den, this.den * that.num)

  def unary_+ = this
  def unary_- = Ratio(-num, den)

  def copy(num: BigInt = num, den: BigInt = den): Ratio = Ratio.apply(num, den)

  def sqrt: Option[Ratio] = {
    def sqrt(x: BigInt) = {
      @tailrec def newton(a: BigInt, depth: Int): Option[BigInt] = {
        if (depth > 20) None
        else if (a * a == x) Some(a)
        else {
          val b = (a + x / a) / 2
          if (a == b) None
          else newton(b, depth+1)
        }
      }
      newton(x >> (x.bitLength / 2), 0)
    }
    if (num < 0) None else if (num == 0) Some(this) else for { n <- sqrt(num); d <- sqrt(den) } yield Ratio(n, d)
  }

  override def toString() = if (den == 1) num.toString else s"$num/$den"
}
object Ratio {
  implicit def apply(a: Int): Ratio = Ratio(a, 1)
  implicit def apply(a: BigInt): Ratio = Ratio(a, 1)
  def apply(num: BigInt, den: BigInt): Ratio = {
    val div = num.gcd(den) * den.signum
    new Ratio(num / div, den / div){}
  }
  def parse(text: String) = {
    val nums = text.split("/")
    if (nums.length == 1) Ratio(BigInt(text)) else Ratio(BigInt(nums(0)), BigInt(nums(1)))
  }
}
class RatioConstruction(a: Int) {
  def /| (b: Int) = Ratio(a, b)
}
object RatioConstruction {
  implicit def apply(a: Int) = new RatioConstruction(a)
}


case class Solution(positions: Seq[Point], facets: Seq[Polygon], destinations: Seq[Point]) {
  def render = {
    List(
      List(positions.length.toString),
      positions.map(_.toString),
      List(facets.length.toString),
      facets.map { facet => (facet.points.length.toString +: facet.points.map(point => positions.indexOf(point).toString)).mkString(" ") },
      destinations.map(_.toString)
    ).flatten.mkString("\n")
  }
}
