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

  lazy val facets: Seq[Polygon] = {
    def extend(origin: Point, set: List[Segment]): Seq[Polygon] = {
      for {
        ns <- connecting(set.head.b)
        if !set.head.colinear(ns.b)
        if !set.exists(_ intersect ns)
        if (set.size >= 2 && ns.b == origin) || !set.exists(_.a == ns.b)
        poly <- if (ns.b == origin) List(Polygon(origin :: set.map(_.b))) else extend(origin, ns :: set)
      } yield poly
    }
    val raw = for { seg <- all; poly <- extend(seg.a, List(seg)) } yield poly
    raw.map(_.normalize).distinct
  }

  def facetsContaining(seg: Segment) = facets.filter(f => f.segments.contains(seg) || f.segments.contains(seg.flip))
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

  def length2 = a dist2 b

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

  def unit = if (a == b) None else length2.sqrt.map(l => Segment(a, (b - a) / l + a))

  def intersect(that: Segment) = {
    Point.ccw(this.a, this.b, that.a) * Point.ccw(this.a, this.b, that.b) < 0 &&
    Point.ccw(that.a, that.b, this.a) * Point.ccw(that.a, that.b, this.b) < 0
  }
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
    Point.ccw(points(0), points(j), points(j + 1))
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

  def normalize = {
    val head = points.min
    if (head == points(0) && twiceArea > 0) this
    else {
      val pl = if (twiceArea > 0) points else points.reverse
      val spot = pl.indexOf(head)
      Polygon(pl.drop(spot) ++ pl.take(spot))
    }
  }

  def segments = Segment(points.last, points.head) :: points.sliding(2).map(p => Segment(p(0),p(1))).toList

  lazy val pointsExtra = points.last +: points

  def inside(p: Point) = {
    var wn = 0
    for {
      j <- 0 until points.length
    } {
      if (pointsExtra(j).y <= p.y) {
        if (pointsExtra(j+1).y > p.y && Point.ccw(pointsExtra(j), pointsExtra(j+1), p) > 0) wn += 1
      } else {
        if (pointsExtra(j+1).y <= p.y && Point.ccw(pointsExtra(j), pointsExtra(j+1), p) < 0) wn += 1
      }
    }
    wn != 0
  }

  def overlap(that: Polygon) = {
    points.filter(p => !that.points.contains(p)).exists(that.inside)
  }
}
object Polygon {
  def parse(text: Iterator[String]) = {
    val count = text.next.toInt
    Polygon((1 to count).map{ _ => Point.parse(text) }.toSeq)
  }
}
case class Point(x: Ratio, y: Ratio) extends Ordered[Point] {
  override def toString() = s"$x,$y"

  def compare(that: Point) = if (this.x == that.x) (this.y - that.y).num.signum else (this.x - that.x).num.signum

  def + (that: Point) = Point(this.x + that.x, this.y + that.y)
  def - (that: Point) = Point(this.x - that.x, this.y - that.y)
  def * (scale: Ratio) = Point(this.x * scale, this.y * scale)
  def / (scale: Ratio) = Point(this.x / scale, this.y / scale)

  def dist2(that: Point) = (this.x - that.x) * (this.x - that.x) + (this.y - that.y) * (this.y - that.y)
}
object Point {
  implicit def apply[T <% Ratio](pair: (T, T)): Point = Point(pair._1, pair._2)
  def parse(text: Iterator[String]): Point = parse(text.next)
  def parse(text: String): Point = {
    val nums = text.split(",")
    Point(Ratio.parse(nums(0)), Ratio.parse(nums(1)))
  }
  def ccw(v0: Point, v1: Point, v2: Point) = {
    (v1.x - v0.x) * (v2.y - v0.y) - (v2.x - v0.x) * (v1.y - v0.y)
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

case class Solution2(points: Map[Point, Point], facets: Seq[Polygon]) {
  def render = {
    val pl = points.keys.toSeq
    List(
      List(pl.size.toString),
      pl.map(_.toString),
      List(facets.length.toString),
      facets.map { facet => (facet.points.length.toString +: facet.points.map(point => pl.indexOf(point).toString)).mkString(" ") },
      pl.map(points).map(_.toString)
    ).flatten.mkString("\n")
  }

  def complete = facets.map(_.twiceArea).sum == Ratio(2)

  def missing(p: Point) = {
    val polys = facets.filter(f => f.points contains p)
    val ins = polys.flatMap(_.segments).filter(_.b == p).map(_.flip)
    val outs = polys.flatMap(_.segments).filter(_.a == p)
    val (i,o) = {
      if (p == Point(0,0)) (ins :+ Segment(p,(1,0))) -> (Segment(p,(0,1)) +: outs)
      else if (p == Point(1,0)) (ins :+ Segment(p,(1,1))) -> (Segment(p,(0,0)) +: outs)
      else if (p == Point(1,1)) (ins :+ Segment(p,(0,1))) -> (Segment(p,(1,0)) +: outs)
      else if (p == Point(0,1)) (ins :+ Segment(p,(0,0))) -> (Segment(p,(1,1)) +: outs)
      else if (p.x == Ratio(0)) (ins :+ Segment(p,(0,0))) -> (Segment(p,(0,1)) +: outs)
      else if (p.x == Ratio(1)) (ins :+ Segment(p,(1,1))) -> (Segment(p,(1,0)) +: outs)
      else if (p.y == Ratio(0)) (ins :+ Segment(p,(1,0))) -> (Segment(p,(0,0)) +: outs)
      else if (p.y == Ratio(1)) (ins :+ Segment(p,(0,1))) -> (Segment(p,(1,1)) +: outs)
      else (ins -> outs)
    }
    val no = o.map(v => v.unit.getOrElse(v).b)
    i.find(q => !no.contains(q.unit.getOrElse(q).b))
  }

  def nextSeg = {
    val pl = points.keys.toSeq.sorted ++ List(Point(0,0),Point(1,0),Point(1,1),Point(0,1))
    pl.view.map(missing).collectFirst { case Some(x) => x }
  }

  def transform(seg: Segment) = Segment(points(seg.a),points(seg.b))
}
