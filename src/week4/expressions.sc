package week4

object expressions {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def eval(e: Expr): Int = e match {
		case Number(n) => n
		case Sum(e1, e2) => eval(e1) + eval(e2)
		case Prod(e1, e2) => eval(e1) * eval(e2)
	}                                         //> eval: (e: week4.Expr)Int
	
	def paren(e:Expr) = {
    e match {
      case Sum(_, _) => "(" + show(e) + ")"
      case _ => show(e)
    }
  }                                               //> paren: (e: week4.Expr)String
	
	def show(e: Expr): String = e match {
		case Number(n) => n.toString
		
		case Var(v) => v
		
		case Sum(e1, e2) => show(e1) + " + " + show(e2)
		
		case Prod(e1, e2) => paren(e1) + " * " + paren(e2)
		
		/*
		case Prod(Sum(e1,e2), Sum(e3, e4)) => "(" + show(Sum(e1,e2)) + ") * (" + show(Sum(e2,e3)) + ")"
		case Prod(Sum(e1,e2), e3) => "(" + show(Sum(e1,e2)) + ") * " + show(e3)
		case Prod(e1, Sum(e2, e3)) => show(e1) + " * (" + show(Sum(e2,e3)) + ")"
		case Prod(e1, e2) => show(e1) + " * " + show(e2)
		*/
	}                                         //> show: (e: week4.Expr)String
	
	eval(Number(5))                           //> res0: Int = 5
	eval(Sum(Number(5), Number(7)))           //> res1: Int = 12
	eval(Prod(Number(4), Sum(Number(5), Number(7))))
                                                  //> res2: Int = 48
 	eval(Prod(Sum(Number(5), Number(7)), Number(4)))
                                                  //> res3: Int = 48
	
	show(Number(5))                           //> res4: String = 5
	
	show(Sum(Number(5), Number(7)))           //> res5: String = 5 + 7
	show(Sum(Number(5), Prod(Number(5), Number(3))))
                                                  //> res6: String = 5 + 5 * 3
 	show(Sum(Prod(Number(5), Number(3)), Number(5)))
                                                  //> res7: String = 5 * 3 + 5
	
	   
	show(Prod(Number(4), Sum(Number(5), Number(7))))
                                                  //> res8: String = 4 * (5 + 7)
	show(Prod(Sum(Number(5), Number(7)), Number(4)))
                                                  //> res9: String = (5 + 7) * 4
	show(Prod(Sum(Number(5), Number(7)), Sum(Number(4), Number(3))))
                                                  //> res10: String = (5 + 7) * (4 + 3)
	show(Prod(Number(5), Number(3)))          //> res11: String = 5 * 3
}

trait Expr
case class Number(n: Int) extends Expr
case class Var(v: String) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr