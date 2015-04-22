object ScalaFun {
	
	//recursive as loop, doesn't keep adding to stack
	def fibonacci(n: Int): Int = {
		// @annotation.tailrec
	  def go(n: Int): Int = 
	    if(n == 0) n
		else if (n == 1) n
		else go(n - 1) + go(n - 2)
	
	  go(n)
	}
	
	//HOF
	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		@annotation.tailrec
		def loop(n: Int): Boolean = {
			if ((as.length - 1) != n) {
				// if (as(n) < as(n + 1)) {
				// 	loop(n + 1, as)
				// }
				if (ordered(as(n), as(n + 1))) {
					loop(n + 1)
				}
				else {
					false
				}
			} 
			else {
				true
			}
		}
		
		loop(0)
	}
	
	//used to sort in above HOF
	def ordered(i: Int, j: Int): Boolean = {
     	if (i < j) true
		else false
	}
	
	def main(args: Array[String]): Unit = {
		print("9th Fib: ")
		println(fibonacci(9))
		// print("println(isSorted(Array(1, 2, 4, 6, 7))): ")
        println(
			isSorted[Int](Array(1, 2, 3), ordered)
			)
		
	    println(
			isSorted[Int](Array(3, 2, 3), ordered)
			)
		// print("println(isSorted(Array(1, 2, 4, 6, 7, 1))): ")
		// println(isSorted(Array(1, 2, 4, 6, 7, 1), sorted()))
	}
}
