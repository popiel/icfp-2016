package com.wolfskeep.icfp2016

import org.scalatest._
import org.scalatest.matchers._
import com.wolfskeep.icfp2016.RatioConstruction._

class RatioSpec extends FunSpec with Matchers {
  describe("Ratio") {
    it("should reduce fractions appropriately") {
      Ratio(2, 4) shouldBe Ratio(1, 2)
      Ratio(6, 4) shouldBe Ratio(3, 2)
      Ratio(27, 6) shouldBe Ratio(9, 2)
      Ratio(1, -1).num shouldBe -1
    }
    it("should add correctly") {
      2 /| 4 + 6 /| 4 shouldBe 2 /| 1
    }
    it("should subtract correctly") {
      2 /| 4 - 6 /| 4 shouldBe -1 /| 1
    }
    it("should multiply correctly") {
      (3 /| 5) * (4 /| 3) shouldBe 4 /| 5
    }
    it("should divide correctly") {
      (2 /| 5) / (1 /| 5) shouldBe 2 /| 1
    }
    it("should convert to string prettily") {
      (2 /| 4).toString shouldBe "1/2"
      (6 /| 3).toString shouldBe "2"
      (-1 /| 1).toString shouldBe "-1"
    }
  }

  describe("Segment") {
    describe("reflection") {
      it("should reflect across a vertical line correctly") {
        Segment((0,0),(0,1)).reflect((1, 1)) shouldBe Point(-1, 1)
        Segment((2,0),(2,1)).reflect((1, 1)) shouldBe Point(3, 1)
      }
      it("should reflect across a horizontal line correctly") {
        Segment((0,0),(1,0)).reflect((1, 1)) shouldBe Point(1, -1)
        Segment((0,2),(1,2)).reflect((1, 1)) shouldBe Point(1, 3)
      }
      it("should reflect across an angled line correctly") {
        Segment((0,1),(1,2)).reflect((1, 0)) shouldBe Point(-1, 2)
        Segment((0,1),(1,3)).reflect((1, 1)) shouldBe Point(-3/|5, 9/|5)
      }
    }
  }

  describe("Polygon") {
    it("should compute a square's area correctly") {
      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1))).twiceArea shouldBe 2 /| 1
      Polygon(List((0, 0), (0, 1), (1, 1), (1, 0))).twiceArea shouldBe -2 /| 1
    }
    it("should compute a triangle's area correctly") {
      Polygon(List((0, 0), (1, 0), (1, 1))).twiceArea shouldBe 1 /| 1
      Polygon(List((0, 0), (0, 1), (1, 1))).twiceArea shouldBe -1 /| 1
    }
    it("should compute a concave polygon's area correctly") {
      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1), (1 /| 2, 1 /| 2))).twiceArea shouldBe 3 /| 2
      Polygon(List((0, 0), (1, 0), (1, 1), (1 /| 2, 1 /| 2), (0, 1))).twiceArea shouldBe 3 /| 2
    }
    it("should check congruency correctly") {
      def beCongruentWith(right: Polygon) = Matcher { (left: Polygon) =>
        MatchResult(left congruent right, s"$left was not congruent with $right", s"$left was congruent with $right")
      }
      def congruentWith(right: Polygon) = BeMatcher { (left: Polygon) =>
        MatchResult(left congruent right, s" was not congruent with $right", s" was congruent with $right")
      }

      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1), (1 /| 2, 1 /| 2))) should beCongruentWith (Polygon(List((0, 0), (0, 1), (1, 1), (1, 0), (1 /| 2, 1 /| 2))))
      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1), (1 /| 2, 1 /| 2))) should beCongruentWith (Polygon(List((1, 0), (2, 0), (2, 1), (1, 1), (3 /| 2, 1 /| 2))))
      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1), (1 /| 2, 1 /| 2))) should beCongruentWith (Polygon(List((0, 1), (1, 1), (1, 0), (0, 0), (1 /| 2, 1 /| 2))))

      Polygon(List((0, 0), (1, 0), (1, 1), (0, 1), (1 /| 2, 1 /| 2))) should not (beCongruentWith (Polygon(List((1, 1), (0, 1), (1, 0), (0, 0), (1 /| 2, 1 /| 2)))))
    }

    it("should check for right angles correctly") {
      Polygon(List((0,0), (1,0), (0,1))) shouldBe 'right
      Polygon(List((0,0), (1,0), (1,1))) should not (be ('right))
    }
  }

  describe("Problem") {
    it("should parse correctly") {
      val text =
"""1
4
0,0
1,0
1,1
0,1
4
0,0 1,0
0,0 0,1
1,0 1,1
0,1 1,1"""

      Problem.parse(text) shouldBe
      Problem(
        Shape(Seq(Polygon(Seq((0,0),(1,0),(1,1),(0,1))))),
        Skeleton(Seq(Segment((0,0),(1,0)),Segment((0,0),(0,1)),Segment((1,0),(1,1)),Segment((0,1),(1,1))))
      )
    }
  }

  describe("Solution") {
    it("should render correctly") {
      Solution(
        List((0,0),(1,0),(1,1),(0,1),(0/|1,1/|2),(1/|2,1/|2),(1/|2,1/|1)),
        List(
          Polygon(List((0,0),(1,0),(1/|2,1/|2),(0/|1,1/|2))),
          Polygon(List((1,0),(1,1),(1/|2,1/|1),(1/|2,1/|2))),
          Polygon(List((0/|1,1/|2),(1/|2,1/|2),(0,1))),
          Polygon(List((1/|2,1/|2),(1/|2,1/|1),(0,1)))
        ),
        List((0,0),(1,0),(0,0),(0,0),(0/|1,1/|2),(1/|2,1/|2),(0/|1,1/|2))
      ).render shouldBe
"""7
0,0
1,0
1,1
0,1
0,1/2
1/2,1/2
1/2,1
4
4 0 1 5 4
4 1 2 6 5
3 4 5 3
3 5 6 3
0,0
1,0
0,0
0,0
0,1/2
1/2,1/2
0,1/2"""
    }
  }
}
