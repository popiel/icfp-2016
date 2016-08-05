package com.wolfskeep.icfp2016

import com.wolfskeep.icfp2016._
import com.wolfskeep.icfp2016.RatioConstruction._
import scala.io._

object FlatCentroid {
  def main(args: Array[String]) {
    val problem = Problem.parse(Source.stdin.getLines)
    val points = problem.shape.polygons.flatMap(_.points).distinct
    val x = points.map(_.x).sum / points.size
    val y = points.map(_.y).sum / points.size
    val solution = Solution(List((0,0),(0,1),(1,1),(1,0)),List(Polygon(List((0,0),(0,1),(1,1),(1,0)))),List(
(x - 1/|2, y - 1/|2),
(x - 1/|2, y + 1/|2),
(x + 1/|2, y + 1/|2),
(x + 1/|2, y - 1/|2)
))

    println(solution.render)
  }
}
