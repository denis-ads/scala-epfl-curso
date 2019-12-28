//Function return function

object Exercises{

  def product(f: Int => Int) (a:Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a +1, b)

  product(x => x * x)(3, 4)


  def fact(n: Int) = product(x => x)(1, n)
  fact(5)

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int )(a: Int, b: Int): Int =
    if (a > b ) zero
    else combine(f(a), mapReduce(f,combine, zero)(a+1, b))

  def product2(f: Int => Int) (a:Int, b: Int): Int = mapReduce(f, (x, y) => x* y, 1)(a, b)
  product(x => x * x)(3, 4)

//}

//object Currying{
  def sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a>b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }

  def sumInts = sum(x=> x)
  def sumCubes = sum(x=> x * x * x)

  def sumFactorials = sum(fact)

  sumCubes(1, 10) + sumFactorials(10, 20)
}

