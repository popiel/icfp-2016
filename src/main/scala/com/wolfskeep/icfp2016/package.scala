package com.wolfskeep

package object icfp2016 {
  implicit object RatioNumeric extends Numeric[Ratio] {
    def compare(x: Ratio, y: Ratio): Int = (x - y).num
    def fromInt(x: Int): Ratio = Ratio(x)
    def minus(x: Ratio, y: Ratio) = x - y
    def negate(x: Ratio) = Ratio(-x.num, x.den)
    def plus(x: Ratio, y: Ratio) = x + y
    def times(x: Ratio, y: Ratio) = x * y
    def toDouble(x: Ratio) = x.num.toDouble / x.den
    def toFloat(x: Ratio) = x.num.toFloat / x.den
    def toInt(x: Ratio) = x.num / x.den
    def toLong(x: Ratio) = x.num / x.den
  }
}
