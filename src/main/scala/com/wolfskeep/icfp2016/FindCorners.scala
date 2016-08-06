package com.wolfskeep.icfp2016

import scala.io._

object FindCorners {
  def main(args: Array[String]) {
    val problem = Problem.parse(Source.stdin.getLines)
    for {
      set <- problem.skeleton.segments.tails
      if set.length >= 2
      head = set.head
      a <- List(head, head.flip)
      other <- set.tail
      b <- List(other, other.flip)
      if a right b
    } {
      println(s"$a $b")
    }
  }
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

  def continuing(from: Segment, across: Segment): Segment = ???
}
