/**
 If the numbers 1 to 5 are written out in words: one, two, three, four, five, 
 	then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
   	in words, how many letters would be used?
*/

import scalaz.syntax.applicative._
import scalaz.std.option._
import scala.util.matching.Regex
import scala.annotation.tailrec

object Problem17 {

	val oneDigit: Regex = "^[1-9]$".r
	val twoDigit: Regex = "^[1-9][0-9]$".r
	val threeDigit: Regex = "^[1-9][0-9][0-9]$".r 
	val fourDigit: Regex = "^[1-9][0-9][0-9][0-9]$".r

	private def isOneToFourDigitNum(x: String): Boolean = 
		x.matches(oneDigit.toString) |
		x.matches(twoDigit.toString) |
		x.matches(threeDigit.toString) |
		x.matches(fourDigit.toString)

    // If all the numbers from 1 to 1000 (one thousand) inclusive were written out 
    // in words, how many letters would be used?
    def runProblem: Option[Int] = {
    	val oneToThousand: List[Int] = (1 to 1000).toList
    	val numberList: List[String] = oneToThousand.map(_.toString)
    	val wordLengths: List[Option[Int]] = numberList.map(getLengthOfMaybeNumberWord)
		wordLengths match {
			case Nil        => None
			case x :: xs    => xs.foldLeft[Option[Int]](x) { 
				(acc: Option[Int], elem: Option[Int]) => ^(acc, elem)(_ + _)
			}
		}
    }


	def getLengthOfMaybeNumberWord(number: String): Option[Int]= {
	  if (isOneToFourDigitNum(number)) {
		val numberAsWord: Option[String] = getNumberAsWord(number)
		val wordFiltered: Option[String] = numberAsWord.flatMap(a => Some(a.filter(x => x != ' ').filter(x => x != '-')))
		wordFiltered.map(_.length)
	  }
	  else None
	}


	def getNumberAsWord(num: String): Option[String] = {
	  	@tailrec
	  	def go(numbers: String)(acc: Option[String]): Option[String] = numbers.toList match {
		    case Nil => acc
		    case a :: b :: c :: d :: Nil =>  {
				val rest: String = b.toString + c.toString + d.toString
				val thousandWord = ^(convertSingleDigitOnes(a), Some(" thousand"))(_ ++ _)
				val newAcc = ^(acc, thousandWord)(_ + _)
				go(rest)(newAcc)
			}
			case b :: c :: d :: Nil => {
				val rest: String = c.toString + d.toString
				val hundredWord = convertSingleDigitHundred(b)
				val newAcc = ^(acc, hundredWord)(_ + _)
				go(rest)(newAcc)
			}
			case '0' :: '0' :: Nil => acc
			case c :: d :: Nil     => acc match {
				case Some("") => {
					val twoDigitsWord = convertTwoDigits(c)(d)
					val newAcc = ^(acc, twoDigitsWord)(_ + _)
					go("")(newAcc)			
				}
				case Some(_: String) => {
					val twoDigitsWord: Option[String] = convertTwoDigits(c)(d)			
					val addingAnd: Option[String] = ^(Some(" and "), twoDigitsWord)(_ + _)
					val newAcc = ^(acc, addingAnd)(_ + _)
					go("")(newAcc)	
				}
				case None => None

			}
			case d :: Nil          => convertSingleDigitOnes(d)
		    case _ 				   => None
    	}	  	 
    	go(num)(Some(""))
	}
 
	private def convertTwoDigits(tens: Char)(ones: Char): Option[String] = (tens, ones) match {
		case ('0', _)  => convertSingleDigitOnes(ones)
		case ('1', _)  => convertTensWithOne(ones)
		case (_, '0')  => convertTens(tens)
		case (_, _)    => ^(convertTens(tens).map(_ ++ "-"), convertSingleDigitOnes(ones))(_ + _)
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
	private def convertSingleDigitOnes(x: Char): Option[String] = x match {
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

		// Given a single Digit in the Ones column, return its corresponding word
	// Example: f(1) -> "one", f(2) -> "two"    
	private def convertSingleDigitHundred(x: Char): Option[String] = x match {
		case '1' => Some("one hundred")
		case '2' => Some("two hundred")
		case '3' => Some("three hundred")
		case '4' => Some("four hundred")
		case '5' => Some("five hundred")
		case '6' => Some("six hundred")
		case '7' => Some("seven hundred")
		case '8' => Some("eight hundred")
		case '9' => Some("nine hundred")
		case '0' => Some("")
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