import scala.collection.immutable.BitSet
import scala.collection.immutable.List
import scala.util.Random

//Item Class
case class Item(
	name: 	Int,
	value: 	Int,
	weight: Int
    ) {
  def this(a: List[Int]) = this(a(0), a(1), a(2))
  override def toString() = name + ": " + value + " " + weight
}

//Cache class
case class CacheElem(
    total: Int,
    elems: List[Item]
    ) extends Ordered[CacheElem]{
  def compare(that: CacheElem) = total.compare(that.total)
  override def toString() = elems.reverse.foldLeft("")((acc, item) => acc ++ item.toString ++ "\n") 
}


//KnapSack Problem Logic
object KnapSack {
  //Functional, implicitly recursive solution to the 0/1 Knapsack problem
  def solve(cap: Int, items: List[Item]) = 
      items.foldLeft(
          List.fill(cap+1)(CacheElem(0, List[Item]()))
          )(
          (cache: List[CacheElem], item: Item)
          => (
              (below: List[CacheElem],above:List[CacheElem])
              => below ++ (
                (above,
                 cache.map(
                    elem 
                    => new CacheElem(item.value + elem.total, item :: elem.elems))
                ).zipped.map(
                      (prev, next)
                      => if (prev > next) prev else next)
                          )
             ).tupled(cache.splitAt(item.weight))
           )(cap)
  
  def main(args: Array[String]) {
    val capacity = readInt()
    //Process input stream into items
    val items = Stream.continually(readLine()).takeWhile(_ != "").toList.map(
        str => new Item(str.trim().replaceAll(":", "").split(" ").map(
        num => num.trim().toInt).toList))
    val res = solve(capacity, items)
    println(res.total)
    println(capacity)
    println(res)
  }
}


//Instance Generator Logic
object InstanceGenerator {
  def generate(param: List[Int]) = param match {
    //Over the top value unpacking but relatively simple.
    case n::cl::cu::vl::vu::wl::wu::_ => (
        (cl to cu+1)(Random.nextInt(cu+1-cl)),
        List.tabulate(n)(i => Item(i+1, randInt(vl, vu), randInt(wl,wu))))
    case _ => (0, List[Item]())
  }
  
  //Random number in range, inclusive low, high
  def randInt(low: Int, high: Int) = (low to high+1)(Random.nextInt(high+1-low))
  
  def main(args: Array[String]) {
    val res = generate(readLine().trim().split(" ").map(x=>x.toInt).toList)
    println(res._1)
    println(res._2.foldLeft("")((acc, s) => acc ++ s.toString ++ "\n"))
  }
}