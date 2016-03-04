package week6

object books {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val books = List(Book("hello", Set("world", "mundo")), Book("hola", Set("world", "monde")), Book("alo", Set("mondo", "mundo")))
                                                  //> books  : List[week6.Book] = List(Book(hello,Set(world, mundo)), Book(hola,Se
                                                  //| t(world, monde)), Book(alo,Set(mondo, mundo)))
  
  def booksOfAuthorWithFor(books: Seq[Book], name: String) =
  	for (b <- books; a <- b.authors if a startsWith name)
 			 yield b.title            //> booksOfAuthorWithFor: (books: Seq[week6.Book], name: String)Seq[String]
 			 
 	def booksOfAuthor(books: Seq[Book], name: String) =
  	books withFilter (b => b.authors exists ( _ startsWith name)) map (_.title)
                                                  //> booksOfAuthor: (books: Seq[week6.Book], name: String)Seq[String]
     
   booksOfAuthor(books, "mundo")                  //> res0: Seq[String] = List(hello, alo)
  
   booksOfAuthorWithFor(books, "mundo")           //> res1: Seq[String] = List(hello, alo)
}

case class Book(title: String, authors: Set[String])