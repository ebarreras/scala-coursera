package week6

import io.Source

object phonenumbers {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
  
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
                                     
                                     

  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  							 '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
  
  def charCode(m: Map[Char,String]): Map[Char, Char] =
  	for (e <- m; v <-e._2) yield (v -> e._1)  //> charCode: (m: Map[Char,String])Map[Char,Char]

	def wordCode(word: String): String =
		word.toUpperCase map charCode(mnem)
                                                  //> wordCode: (word: String)String
 
 	wordCode("Java")                          //> res0: String = 5282
 
  val wordsForNum: Map[String, List[String]] =
  	words groupBy wordCode withDefaultValue List()
                                                  //> wordsForNum  : Map[String,List[String]] = Map(63972278 -> List(newscast), 29
                                                  //| 237638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> Lis
                                                  //| t(allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 8684
                                                  //| 37 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 3364
                                                  //| 646489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 ->
                                                  //|  List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386583
                                                  //|  -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827
                                                  //|  -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 
                                                  //| 84863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464 -> Li
                                                  //| st(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers)
                                                  //| , 46636233 -> List(homemade), 7446768759 -> List(rigorously), 74644647 -> Li
                                                  //| st(ringings), 633738 -> List(offset), 847825 -> List(visual), 772832 -> List
                                                  //| (Pravda), 4729378 -> Lis
                                                  //| Output exceeds cutoff limit.

  	
  def encode(number: String): Set[List[String]] =
    if (number isEmpty())
    	Set(List())
    else {
      (for {
      	i <- 1 to number.length
      	prefix <- wordsForNum(number take i)
      	suffixes <- encode(number drop i)
     	} yield prefix :: suffixes).toSet
  	}                                         //> encode: (number: String)Set[List[String]]
  	
  def translate(number: String): Set[String] =
  	encode(number) map (_ mkString " ")       //> translate: (number: String)Set[String]
  
  //def turn[K, V](m: Map[K,V]): Map[V, K] =
  //	for (e <- m) yield (e._2 -> e._1)
  //turn(Map(1 -> 'A', 2 -> 'B', 2 -> 'C'))
  	
  charCode(mnem)                                  //> res1: Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -> 5, 
                                                  //| U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5, B -
                                                  //| > 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -> 9
                                                  //| , S -> 7)
  translate("7225247386")                         //> res2: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)
}