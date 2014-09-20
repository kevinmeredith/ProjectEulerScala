/**
 If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
 	then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
   	in words, how many letters would be used?
*/

import scalaz.syntax.applicative._
import scalaz.std.option._
import scala.util.matching.Regex

object Problem17 {

	type Length = Int

	val oneDigit: Regex = "^[1-9]$".r
	val twoDigit: Regex = "^[1-9][0-9]$".r
	val threeDigit: Regex = "^[1-9][0-9][0-9]$".r 
	val fourDigit: Regex = "^[1-9][0-9][0-9][0-9]$".r

	private def isOneToFourDigitNum(x: String): Boolean = 
		x.matches(oneDigit.toString) |
		x.matches(twoDigit.toString) |
		x.matches(threeDigit.toString) |
		x.matches(fourDigit.toString)

	def runProblem(number: String): Option[Length]= {
	  if (isOneToFourDigitNum(number)) getLetterCount(number).filter(x => x != ' ' && x != '-').map(_.length)
	  else 				               None
	}

	def getLetterCount(numbers: String): Option[String] = numbers.toList match {
	    case a :: '0' :: '0' :: '0' :: Nil =>  {
	    	^(convertSingleDigit(a), Some(" thousand"))(_ ++ _)
	    }
	    case a :: b :: c :: d :: Nil =>  {
			val rest: String = b.toString + c.toString + d.toString
			val thousandWord = ^(convertSingleDigit(a), Some(" thousand"))(_ ++ _)
			^(Some(a + " thousand"), getLetterCount(rest))(_ + _) 
		}
		case b :: '0' :: '0' :: Nil =>  
			^(convertSingleDigit(b), Some(" hundred"))(_ + _) 
	    case b :: c :: d :: Nil =>  {
			val rest: String = c.toString + d.toString
			val hundredWord = convertSingleDigit(b).map(_ ++ " hundred and ")
			^(hundredWord, getLetterCount(rest))(_ + _) 							
		}
		case '0' :: '0' :: Nil => Some("")
		case c :: '0' :: Nil   => convertTens(c)			
		case '0' :: d :: Nil   => convertSingleDigit(d) 
		case '1' :: d :: Nil   => convertTensWithOne(d)
	    case c :: d :: Nil     => {
	    	val tensWithDash: Option[String] = ^(convertTens(c), Some("-"))(_ + _) 
	    	^(tensWithDash, convertSingleDigit(d))(_ + _)  
	    }
		case d :: Nil          => convertSingleDigit(d)
	    case _ 				   => None
    }   
 
    // Converts a 2-digit number to its word equivalent *where* "1" is in the tens column
    // Examples: 15, 12, 18, etc.
	private def convertTensWithOne(ones: Char): Option[String] = ones match {
		case '0' => Some("ten")
		case '1' => Some("eleven")
		case '2' => Some("twelve")
		case '3' => Some("thirteen")
		case '4' => Some("fourteen")
		case '5' => Some("fifteen")
		case '6' => Some("sixteen")
		case '7' => Some("seventeen")
		case '8' => Some("eighteen")
		case '9' => Some("nineteen")
		case _    => None
	}

	// Given a single Digit in the Ones column, return its corresponding word
	// Example: f(1) -> "one", f(2) -> "two"    
	private def convertSingleDigit(x: Char): Option[String] = x match {
		case '1' => Some("one")
		case '2' => Some("two")
		case '3' => Some("three")
		case '4' => Some("four")
		case '5' => Some("five")
		case '6' => Some("six")
		case '7' => Some("seven")
		case '8' => Some("eight")
		case '9' => Some("nine")
		case '0' => Some("zero")
		case  _  => None
	}

	// Given a single Digit in the Tens column, return its corresponding word.
    // Example: f(2) -> "twenty", f(5) = "fifty"
	// Due to 1's uniqueness, i.e. 15 = "fifteen", not "ten-five", it's handled in 
	// `convertTensWithOne`.
	private def convertTens(x: Char): Option[String] = x match {
		case '2' => Some("twenty")
		case '3' => Some("thirty")
		case '4' => Some("forty")
		case '5' => Some("fifty")
		case '6' => Some("sixty")
		case '7' => Some("seventy")
		case '8' => Some("eighty")
		case '9' => Some("ninety")
		case _   => None
	}
}