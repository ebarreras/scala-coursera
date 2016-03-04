package week6

object queens {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    ((row - 1 to 0 by -1 ) zip queens).forall { case (r, c) => col != c && Math.abs(col - c) != row - r }
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean

  def queens(n: Int): List[List[Int]] = {
    def placeQueens(row: Int): List[List[Int]] =
      if (row == n)
        List(List())
      else {
        val smallerSolutions = placeQueens(row + 1)
        for {
          smaller <- smallerSolutions
          c <- 0 until n
          if isSafe(c, smaller)
        } yield c :: smaller
      }

    placeQueens(0)
  }                                               //> queens: (n: Int)List[List[Int]]

  
  def showQueens(queens: List[Int]) = {
  	val size = queens.length
 		val reversedRows = queens.reverse
 		reversedRows map (c => (0 until size).map(i => if (i == c) "X" else "_").mkString(" ")) mkString ("\n")
  }                                               //> showQueens: (queens: List[Int])String
  
  queens(4)(0)                                    //> res0: List[Int] = List(2, 0, 3, 1)
  
  showQueens(queens(4)(0))                        //> res1: String = _ X _ _
                                                  //| _ _ _ X
                                                  //| X _ _ _
                                                  //| _ _ X _
                   
  "\n" + (queens(4) map (showQueens(_)) mkString ("\n\n"))
                                                  //> res2: String = "
                                                  //| _ X _ _
                                                  //| _ _ _ X
                                                  //| X _ _ _
                                                  //| _ _ X _
                                                  //| 
                                                  //| _ _ X _
                                                  //| X _ _ _
                                                  //| _ _ _ X
                                                  //| _ X _ _"
 }