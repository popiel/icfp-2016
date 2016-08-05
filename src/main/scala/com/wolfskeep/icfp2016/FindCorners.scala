package com.wolfskeep.icfp2016

import scala.io._

object Main {
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
