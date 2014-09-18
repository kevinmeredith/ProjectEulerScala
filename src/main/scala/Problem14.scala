/*
The following iterative sequence is defined for the set of positive Longegers:

n → n/2 (n is even)
n → 3n + 1 (n is odd)

Using the rule above and starting with 13, we generate the following sequence:

13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
*/
import scala.annotation.tailrec

object Collatz { 

	def reverse[T](xs: List[T]): List[T] = xs.foldLeft(List[T]()){ (acc, elem) => elem :: acc }

	def isEven(n: Long): Boolean = {
		if ( n < 0 ) false      // correct?
		else if (n % 2 == 0) true
		else false
	}

	def collatz(n: Long): List[Long] = {
		if ( n == 1 ) 1 :: Nil
		else if ( isEven(n) ) n :: collatz(n / 2) 
		else n :: collatz( 3*n + 1 )
	}

	def collatzTailRecursive(n: Long): List[Long] = {
		@tailrec
		def go(x: Long, acc: List[Long]): List[Long] = {
			if ( x == 1 )         reverse(1 :: acc)
			else if ( isEven(x) ) go(x / 2, x :: acc) 
			else                  go(3*x + 1, x :: acc) 
		}
		go(n, Nil)
	}

    // http://stackoverflow.com/questions/25881787/generically-finding-max-item-in-list
	def getMax(ys: List[Long]): Option[Long] = ys match {
		case Nil 	 => None
		case x :: xs => { 
			val result = xs.foldLeft(x: Long){ (acc: Long, elem:Long) => if (acc > elem) acc else elem }
			Some(result)
		}
	}

	type Index = Long
	type Length = Long
	
	/**
	 * Given a List of tuples (Index, List[Long]), return the optional element (Index, List[Long]) 
	 * with the longest length list.
	 */
	def getMaxWithIndex(ys: Stream[(Index, List[Long])]): Option[(Index, List[Long])] = ys match {
		case Stream() 	 => None
		case x #:: xs     => { 
			val result = xs.foldLeft(x: (Index, List[Long])){ 
				(acc: (Index, List[Long]), elem: (Index, List[Long])) => if (acc._2.length > elem._2.length) acc else elem 
			}
			Some(result)
		}
	}

    // Which starting number, under one million, produces the longest chain?
    // This approach failed due to running out of memory!
	def runProblem(): Option[Long] = {
		def list1toMil: Stream[(Index,Long)] = (0L to 333333L).toList.zip(0L to 333333L).toStream
		def collatzes: Stream[(Index, List[Long])] = list1toMil.flatMap{x => List((x._1, collatzTailRecursive(x._2)))}
		getMaxWithIndex(collatzes) match {
			case None           => None
			case Some((index, _)) => Some(index)
		}
	}

	def runProblem2(): (Long, Long) = {
		@tailrec
		def go(x: Long, max: (Index, Length)): (Index, Length) = {
			lazy val xLen = collatzTailRecursive(x).length 

			if(x == 1000000L)      max
			else if(xLen > max._2) go(x+1, (x, xLen))
			else                   go(x+1, max)
		}
		go(1, (0,0))
	}
}