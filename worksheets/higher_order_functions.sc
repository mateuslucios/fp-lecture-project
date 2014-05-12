object higher_order_functions {

  // Auxiliar functions
  def fact(x: Int): Int =
    if (x == 0) 1 else if (x <= 2) x else x * fact(x - 1)
                                                  //> fact: (x: Int)Int
  // Sum functions
  //x => x is a function that given x return x
  def sumInts(a: Int, b: Int): Int =
    sum(x => x, a, b)                             //> sumInts: (a: Int, b: Int)Int

  //x => x * x * x  is a function that given x return pow(x, 3)
  def sumCubes(a: Int, b: Int): Int =
    sum(x => x * x * x, a, b)                     //> sumCubes: (a: Int, b: Int)Int

  def sumFactorials(a: Int, b: Int): Int =
    sum(fact, a, b)                               //> sumFactorials: (a: Int, b: Int)Int

  def sum(f: Int => Int, a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f, a + 1, b)     //> sum: (f: Int => Int, a: Int, b: Int)Int

  def _sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> _sum: (f: Int => Int)(a: Int, b: Int)Int

  def __sum(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int =
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    sumF
  }                                               //> __sum: (f: Int => Int)(Int, Int) => Int

  def _sumInts = __sum(x => x)                    //> _sumInts: => (Int, Int) => Int

  //x => x * x * x  is a function that given x return pow(x, 3)
  def _sumCubes = __sum(x => x * x * x)           //> _sumCubes: => (Int, Int) => Int

/*
 A function that returns:
 - an anonimous function that takes two ints and returns an int
*/
  def _sumFactorials: (Int, Int) => Int = __sum(fact)
                                                  //> _sumFactorials: => (Int, Int) => Int

  sumInts(1, 3)                                   //> res0: Int = 6
  sumCubes(1, 3)                                  //> res1: Int = 36
  sumFactorials(1, 3)                             //> res2: Int = 9

  _sumInts(1, 3)                                  //> res3: Int = 6
  _sumCubes(1, 3)                                 //> res4: Int = 36
  _sumFactorials(1, 3)                            //> res5: Int = 9

  _sum(x => x)(1, 3)                              //> res6: Int = 6
  _sum(x => x * x * x)(1, 3)                      //> res7: Int = 36
  _sum(fact)(1, 3)                                //> res8: Int = 9
  
  __sum(x => x*x*x)(1, 3)                         //> res9: Int = 36
}