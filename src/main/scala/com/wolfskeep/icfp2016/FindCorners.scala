package com.wolfskeep.icfp2016

import scala.io._

object FindCorners {
  def main(args: Array[String]) {
    val problem = Problem.parse(Source.stdin.getLines)
    println(new Solver(problem).squaresFromCorners.head.render)
  }
}

case class PathStep(reflections: Seq[Segment], segment: Segment)
case class Path(segment: Segment, steps: Seq[PathStep]) {
  lazy val transformed = steps.foldLeft(segment.a){ (p: Point, step: PathStep) =>
    step.reflections.foldLeft(p){ (p2: Point, axis: Segment) =>
      axis reflect p2
  } }
  def length2 = {
    if (steps.isEmpty) segment.length2
    else Segment(transformed, steps.last.segment.b).length2
  }
  def lastSegment = steps.lastOption.map(_.segment).getOrElse(segment)
  def append(step: PathStep) = copy(steps = steps :+ step)
}

class Solver(problem: Problem) {
  lazy val rightAngles: Seq[(Segment, Segment)] = {
    for {
      set <- problem.skeleton.segments.tails
      if set.length >= 2
      head = set.head
      a <- List(head, head.flip)
      other <- set.tail
      b <- List(other, other.flip)
      if a right b
    } yield (a, b)
  }.toSeq

  lazy val rightAngles2: Seq[(Segment, Segment)] = {
    for {
      set <- problem.skeleton.segments.tails
      if set.length >= 2
      head = set.head
      a <- List(head, head.flip)
      other <- set.tail
      b <- List(other, other.flip)
      if a right b
    } yield (a, b)
  }.toSeq

  def connecting(from: Segment): Seq[Segment] = {
    for {
      seg <- problem.skeleton.segments ++ problem.skeleton.segments.map(_.flip)
      if from.b == seg.a
    } yield seg
  }

  def continuing(from: Segment): Seq[(Segment,Segment)] = {
    val others = connecting(from)
    for {
      across <- others
      if !(from colinear across.b)
      other <- others
      if from colinear across.reflect(other.b)
    } yield (across, other)
  }

  def expandPath(path: Path) {
    if (path.length2 == Ratio(1)) {
      println(path)
    } else if (path.length2 < 1) {
      val others = connecting(path.lastSegment).toSet
      def attempt(subset: Seq[Segment]) {
        val transformed = subset.foldLeft(path.transformed){ (p: Point, s: Segment) => s reflect p }
        val remain = others -- subset
        for {
          seg <- remain
          if seg colinear transformed
        } expandPath(path.append(PathStep(subset, seg)))
        for {
          axis <- remain
          if !(axis colinear transformed)
        } attempt(subset :+ axis)
      }
      attempt(Seq.empty)
    }
  }

  def findPaths() {
    for {
      (a, b) <- rightAngles
      o <- List(a, b)
    } expandPath(Path(o, Nil))
  }
/*
  def findRemainder(path: Seq[(Segment,Segment)], acc: Seq[Seq[(Segment,Segment)]]): Seq[Seq[(Segment,Segment)]] = {
    val p = path.foldLeft
    if (remain.num < 0) acc
    else if (remain.num == 0) path +: acc
    else {
      val options = continuing(path.head._2)
      
      options.map(next => findRemainder(remain - next._2.length2.
    }
  }
*/

  def transform(fromSegment: Segment, fromPoint: Point, toSegment: Segment): Seq[Point] = {
    val l1 = Segment(fromSegment.a, fromPoint).length2
    val l2 = Segment(fromSegment.b, fromPoint).length2
    val x1 = toSegment.a.x
    val x2 = toSegment.b.x
    val y1 = toSegment.a.y
    val y2 = toSegment.b.y
    if (y1 == y2) {
      val q = (l2 - l1 - (x1 - x2) * (x1 - x2) + y1 * y1 - y2 * y2) / ((x1 - x2) * 2)
      val r = (y1 - y2) / (x1 - x2)
      val a = r * r + 1
      val b = (y1 + q * r) * -2
      val c = y1 * y1 + q * q - l1
      val det = b * b - a * c * 4
      for {
        pos <- det.sqrt.toSeq
        n <- Seq(pos, -pos)
        y0 = (n - b) / a / 2
        det2 = (l1 - (y0 - y1) * (y0 - y1))
        p <- det2.sqrt.toSeq
        o <- Seq(p, -p)
        x0 = x1 + o
        point = Point(x0, y0)
        if Segment(toSegment.a, point).length2 == l1
        if Segment(toSegment.b, point).length2 == l2
      } yield point
    } else {
      val q = (l2 - l1 - (y1 - y2) * (y1 - y2) + x1 * x1 - x2 * x2) / ((y1 - y2) * 2)
      val r = (x1 - x2) / (y1 - y2)
      val a = r * r + 1
      val b = (x1 + q * r) * -2
      val c = x1 * x1 + q * q - l1
      val det = b * b - a * c * 4
      for {
        pos <- det.sqrt.toSeq
        n <- Seq(pos, -pos)
        x0 = (n - b) / a / 2
        p <- (l1 - (x0 - x1) * (x0 - x1)).sqrt.toSeq
        o <- Seq(p, -p)
        y0 = y1 + o
        point = Point(x0, y0)
        if Segment(toSegment.a, point).length2 == l1
        if Segment(toSegment.b, point).length2 == l2
      } yield point
    }
  }

  def squaresFromCorners: Seq[Solution] = {
    for {
      angle <- rightAngles
      l1 <- angle._1.length2.sqrt.toSeq
      l2 <- angle._2.length2.sqrt.toSeq
      p0 = Point(0, 0)
      p1 = Point(l1, 0)
      p2 = Point(0, l2)
      v0 = angle._1.a
      s1 <- angle._1.unit.toSeq
      s2 <- angle._2.unit.toSeq
      v1 = s1.b
      v2 = s2.b
      v3 <- transform(Segment((0,0),(1,0)),(1,1),s1) intersect transform(Segment((0,0),(0,1)),(1,1),s2)
    } yield Solution(List((0,0),(1,0),(1,1),(0,1)),List(Polygon(List((0,0),(1,0),(1,1),(0,1)))),List(v0,v1,v3,v2))
  }
}
