object ProblemSet1 {
    def drop[A](n: Int, xs: List[A]): List[A] = {
      for (i <- List.range(0, xs.length) if (i+1) % n !=0 )
        yield xs(i)
    }
    
    def rotate[T](n: Int, xs: List[T]):List[T] = {
      if (n > 0){
        val (first, last) = List.range(0, xs.length) span(_ < n)
        for(i <- (last ++ first)) yield xs(i)
      }
      else{
        val (first, last) = List.range(0, xs.length) span (_ < xs.length + n)
    	for(i <- (last ++ first)) yield xs(i)
      }
    }
   
    def join[A,B](xs: List[A], ys: List[B], p: (A, B) => Boolean):List[(A,B)] = {
       for(i <- xs; j <- ys if p(i,j)) yield (i,j)
    }
}