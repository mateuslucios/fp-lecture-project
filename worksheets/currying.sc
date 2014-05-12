object currying {

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:
                                                  //|  Int)Int

  def _product(f: Int => Int)(a: Int, b: Int): Int =
    mapReduce(f, (x, y) => x * y, 1)(a, b)        //> _product: (f: Int => Int)(a: Int, b: Int)Int

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x)(1, 3)                           //> res0: Int = 6
  product(x => x)(1, 4)                           //> res1: Int = 24
  product(x => x)(1, 5)                           //> res2: Int = 120

  _product(x => x)(1, 3)                          //> res3: Int = 6
  _product(x => x)(1, 4)                          //> res4: Int = 24
  _product(x => x)(1, 5)                          //> res5: Int = 120

  def fact(x: Int) = product(x => x)(1, x)        //> fact: (x: Int)Int

  fact(5)                                         //> res6: Int = 120

}