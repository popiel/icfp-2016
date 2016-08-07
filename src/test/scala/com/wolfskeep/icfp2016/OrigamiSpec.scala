package com.wolfskeep.icfp2016

import org.scalatest._
import org.scalatest.matchers._
import com.wolfskeep.icfp2016.RatioConstruction._

class RatioSpec extends FunSpec with Matchers with Inspectors {
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
    it("should provide sqrt when possible") {
      (0 /| 1).sqrt shouldBe Some(0 /| 1)
      (1 /| 1).sqrt shouldBe Some(1 /| 1)
      (9 /| 4).sqrt shouldBe Some(3 /| 2)
      Ratio(BigInt(121), BigInt("1524157875323883675019051998750190521")).sqrt shouldBe Some(Ratio(BigInt(11), BigInt("1234567890123456789")))
    }
    it("should not provide imaginary sqrts") {
      (-9 /| 4).sqrt shouldBe None
    }
    it("should not provide irrational sqrts") {
      (2 /| 1).sqrt shouldBe None
      (1020 /| 1).sqrt shouldBe None
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
    describe("unit") {
      it("should rescale vectors when the result is rational") {
        Segment((1,0),(1/|1,1/|2)).unit shouldBe Some(Segment((1,0),(1,1)))
      }
      it("should not rescale vectors when the result is irrational") {
        Segment((1,0),(1/|2,1/|2)).unit shouldBe None
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

    it("should check for overlap correctly") {
      val p1 = Polygon(List((0,0), (1,0), (1,1), (0,1)))
      val p2 = Polygon(List((2,0), (1,0), (1,1), (2,1)))
      p1 overlap p2 shouldBe false
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

  describe("Skeleton") {
    describe("facets") {
      it("should make a single facet for simple squares") {
        Problem.parse(1).skeleton.facets should have length 1
        Problem.parse(5).skeleton.facets should have length 1
      }
    }
  }

  describe("Solution2") {
    it("should start incomplete") {
      Solution2(Map.empty, Nil).complete shouldBe false
    }
    it("should stay incomplete when only half done") { pending }
  }

  describe("Solver") {
    val simple =
      Problem(
        Shape(Seq(Polygon(Seq((0,0),(1,0),(1,1),(0,1))))),
        Skeleton(Seq(Segment((0,0),(1,0)),Segment((0,0),(0,1)),Segment((1,0),(1,1)),Segment((0,1),(1,1))))
      )
    val canted = Problem.parse(5)
    val noCorners = Problem.parse(1138)

    it("should be able to transform points from one segment to another") {
      val s = new Solver(simple)
      s.transform(Segment((0,0),(1,0)),Point(2,2),Segment((1,1),(2,1))) should contain theSameElementsAs List(Point(3,3),Point(3,-1))
    }

    it("should be able to transform points from one segment to another when the segments are the same") {
      val s = new Solver(simple)
      s.transform(Segment((0,0),(1,0)),Point(0,1),Segment((0,0),(1,0))) should contain theSameElementsAs List(Point(0,1),Point(0,-1))
    }
    it("should be able to transform points from one segment to another when the segments are the same, aligned with the terminus") {
      val s = new Solver(simple)
      s.transform(Segment((0,0),(1,0)),Point(1,1),Segment((0,0),(1,0))) should contain theSameElementsAs List(Point(1,1),Point(1,-1))
    }
    it("should be able to transform points from one segment to another when the segments are the same, the borked case") {
      val s = new Solver(simple)
      val (z,h,o) = (0/|1,1/|2,1/|1)
      s.transform(Segment((z,h),(h,h)),Point(z,z),Segment((z,h),(h,h))) should contain theSameElementsAs List(Point(z,z),Point(z,o))
    }

    it("should be able to transform a polygon from one segment to another") {
      val s = new Solver(simple)
      val p1 = Polygon(List((0,0),(1,0),(1,1),(0,1)))
      val from = Segment((0,0),(1,0))
      val to   = Segment((0,1),(1,1))
      val m1 = Polygon(List((0,1),(1,1),(1,2),(0,2)))
      val m2 = Polygon(List((0,1),(1,1),(1,0),(0,0)))
      s.transform(from, p1, to) should contain theSameElementsAs List(m1, m2)
    }

    it("should be able to solve a simple rotation") {
      val s = new Solver(canted)
      val answers = s.squaresFromCorners
      atLeast(1, answers.map(_.destinations)) should contain theSameElementsAs canted.shape.polygons(0).points
    }

    it("should be able to solve a simple size reduction") {
      val problem = Problem.parse(8)
      val s = new Solver(problem)
      val answers = s.makeTiling
      val answer = s.makeTiling.head
//      println(answer)
      forAll(problem.shape.polygons.flatMap(_.points)) { point =>
        answer.points.values should contain (point)
      }
    }
  }
}
