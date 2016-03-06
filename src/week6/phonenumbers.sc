package week6

import io.Source

object phonesnumbers {
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
     
  val uWords =
  	words groupBy (_.toUpperCase)             //> uWords  : scala.collection.immutable.Map[String,List[String]] = Map(REFRESHI
                                                  //| NGLY -> List(refreshingly), UNSTEADY -> List(unsteady), PROVINCES -> List(pr
                                                  //| ovinces), ABIGAIL -> List(Abigail), KITES -> List(kites), OBSCURER -> List(o
                                                  //| bscurer), STILLER -> List(stiller), ENGRAVE -> List(engrave), REBIND -> List
                                                  //| (rebind), BRAINSTEMS -> List(brainstems), BIRMINGHAMIZES -> List(Birminghami
                                                  //| zes), ETCHING -> List(etching), KNOTS -> List(knots), COCO -> List(coco), CI
                                                  //| TYWIDE -> List(citywide), FLUTTERS -> List(flutters), INESTIMABLE -> List(in
                                                  //| estimable), COLONIZATION -> List(colonization), PHONEMIC -> List(phonemic), 
                                                  //| TOILETS -> List(toilets), ASSISTANCES -> List(assistances), PURIFIES -> List
                                                  //| (purifies), LUSTS -> List(lusts), RECURSES -> List(recurses), WONDERING -> L
                                                  //| ist(wondering), BLARING -> List(blaring), STITCH -> List(stitch), MINK -> Li
                                                  //| st(mink), BLACKMAIL -> List(blackmail), PETTINESS -> List(pettiness), MANICU
                                                  //| RE -> List(manicure), RO
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
 	
	def wordsForNum(number: String) : List[String] = {
	  def expand(number: String): List[String] =
		  if (number.isEmpty)
		  	List("")
		  else {
		  	(for {
		  		c <- mnem.getOrElse(number.head, number.head.toString)
		  		suffix <- expand(number.tail)
		  	} yield c + suffix).toList
	 	 }
	  val words = expand(number)
	  words filter (uWords contains _) flatMap (uWords.getOrElse(_, List()))
	}                                         //> wordsForNum: (number: String)List[String]
	  wordsForNum("2229")                     //> res1: List[String] = List(Abby, baby)
  	
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
  
  charCode(mnem)                                  //> res2: Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -> 5, 
                                                  //| U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5, B -
                                                  //| > 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -> 9
                                                  //| , S -> 7)
  translate("11111112229")                        //> res3: Set[String] = Set()
  translate("7225247386")                         //> res4: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)
}