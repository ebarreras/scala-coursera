package chapter1

import math._

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(95); 
  println("Welcome to the Scala worksheet");$skip(4); val res$0 = 
  3;System.out.println("""res0: Int(3) = """ + $show(res$0));$skip(4); val res$1 = 
  4;System.out.println("""res1: Int(4) = """ + $show(res$1));$skip(141); 

  def sqrtIter(a: Double, x: Double): Double = {
    if (abs(a * a - x) < x*0.0001)
      a
    else
      sqrtIter((a + x / a) / 2, x)
  };System.out.println("""sqrtIter: (a: Double, x: Double)Double""");$skip(58); 

  def sqrt(x: Double): Double = {
    sqrtIter(1, x)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(15); val res$2 = 


	sqrt(10e-6);System.out.println("""res2: Double = """ + $show(res$2));$skip(15); val res$3 = 
	
	sqrt(10e48);System.out.println("""res3: Double = """ + $show(res$3))}
	
}
