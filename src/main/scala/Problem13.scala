/*
The following iterative sequence is defined for the set of positive integers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
*/
import scala.annotation.tailrec

object Collatz { 

	def reverse[T](xs: List[T]): List[T] = xs.foldLeft(List[T]()){ (acc, elem) => elem :: acc }

	def isEven(n: Int): Boolean = {
		if ( n < 0 ) false      // correct?
		else if (n % 2 == 0) true
		else false
	}

	def collatz(n: Int): List[Int] = {
		if ( n == 1 ) 1 :: Nil
		else if ( isEven(n) ) n :: collatz(n / 2) 
		else n :: collatz( 3*n + 1 )
	}

	def collatzTailRecursive(n: Int): List[Int] = {
		@tailrec
		def go(x: Int, acc: List[Int]): List[Int] = {
			if ( x == 1 ) reverse(1 :: acc)
			else if ( isEven(x) ) go(x / 2, x :: acc) 
			else  go(3*x + 1, x :: acc) 
		}
		go(n, Nil)
	}

    // http://stackoverflow.com/questions/25881787/generically-finding-max-item-in-list
	def getMax(ys: List[Int]): Option[Int] = ys match {
		case Nil 	 => None
		case x :: xs => { 
			val result = xs.foldLeft(x: Int){ (acc: List[Int], elem:Int) => if (acc > elem) acc else elem }
			Some(result)
		}
	}


    // Which starting number, under one million, produces the longest chain?
	def runProblem(n: Int): Option[Int] = {
		val list1toMil: List[Int] = (0 to 999999).toList
		val collatzes: List[Int] = list1toMil.map(collatzTailRecursive)
		getMax(collatzes)
	}
}