package com.wolfskeep.icfp2016

import org.scalatest._
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
    }
  }
}