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
  import problem.skeleton._

  def continuing(from: Segment): Seq[(Segment,Segment)] = {
    val others = connecting(from.b)
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
      val others = connecting(path.lastSegment.b).toSet
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

  def usableCorners: Seq[(Segment, Segment)] = {
    for {
      angle <- rightAngles
      l1 <- angle._1.length2.sqrt.toSeq
      l2 <- angle._2.length2.sqrt.toSeq
    } yield angle
  }

  def transform(fromSeg: Segment, fromPoly: Polygon, toSeg: Segment): Seq[Polygon] = {
    val key = fromPoly.points.find(p => !(fromSeg colinear p)).get
    for {
      lock <- transform(fromSeg, key, toSeg)
    } yield Polygon(for {
      p <- fromPoly.points
    } yield (for {
      t <- transform(fromSeg, p, toSeg)
      if (lock dist2 t) == (key dist2 p)
    } yield t).head)
  }

  def expand(solution: Solution2): Seq[Solution2] = {
    if (solution.complete) List(solution)
    else {
      solution.nextSeg match {
        case None => List(solution)
        case Some(nextSeg) =>
println("expanding: " + solution)
          val image = solution.transform(nextSeg)
          val fs = facetsContaining(image)
          for {
            facet <- fs
            seg = (if (facet.segments.contains(image)) image else image.flip)
            sf <- transform(seg, facet, nextSeg)
            if (!sf.points.exists(p => p.y < 0 || p.y > 1 || p.x < 0 || p.x > 1))
            if !solution.facets.exists(_ overlap sf)
            pointmap = (sf.points zip facet.points).toMap
            sol <- expand(Solution2(solution.points ++ pointmap, sf +: solution.facets))
          } yield sol
      }
    }
  }

  def makeTiling: Seq[Solution2] = {
    (for {
      corner <- usableCorners
      seg = corner._1
      nextSeg = Segment((0,0),(corner._1.length2.sqrt.get,Ratio(0)))
      facet <- facetsContaining(seg)
      sf <- transform(seg, facet, nextSeg)
      if (!sf.points.exists(p => p.y < 0 || p.y > 1 || p.x < 0 || p.x > 1))
_ = println(s"transformed $facet to $sf")
      pointmap = (sf.points zip facet.points).toMap
      base = Solution2(pointmap, List(sf))
    } yield base).distinct.flatMap(expand)
  }
}
