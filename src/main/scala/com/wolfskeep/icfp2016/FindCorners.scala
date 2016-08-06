package com.wolfskeep.icfp2016

import scala.io._

object FindCorners {
  def main(args: Array[String]) {
    val problem = Problem.parse(Source.stdin.getLines)
    new Solver(problem).findPaths()
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
}
