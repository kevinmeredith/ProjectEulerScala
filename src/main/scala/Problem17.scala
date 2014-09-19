/**
 If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
 	then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
   	in words, how many letters would be used?
*/

val one   = (1, "one")
val two   = (2, "two")
val three = (3, "three")
val four  = (4, "four")
val five  = "five"
val six   = "six"
val seven = "eve"


def getLetterCount(numbers: String): Option[String] = numbers match {
	case Nil     => None
    case x :: xs => { 
    	if (x.length == 4) convertNumToWord(x).map(x ++ )
    }

}
