import scala.collection.BitSet

/**
  * Created by Andi on 03/04/2017.
  */
object Stegosaurus extends App {

  val pairs: List[(Char, Char)] = List(
    ('A', 'Α'), // greek capital letter alpha
    ('B', 'Β'), // greek capital letter beta
    ('E', 'Ε') // greek capital letter epsilon
  )

  val normalKeys = pairs.map(_._1).toSet
  val changedKeys = pairs.map(_._2).toSet
  val encodeMap = pairs.toMap
  val decodeMap = pairs.map(_.swap).toMap

  def size(s: String): Int = s.count(normalKeys)

  val input = "AAAAAAAAAAAA"
  println(s"the amount of data I can fit in '$input' is ${size(input)}")

  trait Letter
  case class Swappable(c: Char) extends Letter
  case class IndexedSwappable(c: Char, idx: Int) extends Letter
  case class Unswappable(c: Char) extends Letter

  case class Grouping(s: Swappable, us: List[Unswappable])

  def encode(s: String, b: BitSet): Option[String] = {
    if(b.size < size(s)) {
      val partitionedInput: List[Letter] = s.toList.map { c =>
        if (normalKeys contains c) {
          Swappable(c)
        } else Unswappable(c)
      }
      val (indexedCount, numbered) = partitionedInput.foldLeft((0, List.empty[Letter])) { case ((i, ls), nextLetter) =>
        nextLetter match {
          case Swappable(c) => (i + 1, IndexedSwappable(c, i) :: ls)
          case u@Unswappable(_) => (i, u :: ls)
        }
      }
      Some(numbered.reverse.flatMap {
        case IndexedSwappable(c, i) => if (b.contains(i)) encodeMap.get(c) else Some(c)
        case Unswappable(c) => Some(c)
        case _ => None
      }.mkString)
    } else None
  }

  println("trying an encode")
  val data = BitSet(1, 2, 5, 6, 7, 8)
  val output = encode(input, data)
  println(s"encoded: $output")

  def decode(s: String): BitSet = {
    s.flatMap {
      case c if normalKeys contains c => Some(false)
      case c if changedKeys contains c => Some(true)
      case _ => None
    }.zipWithIndex.foldLeft(BitSet.empty) { case (b, (state, index)) =>
      if (state) {
        b.+(index)
      } else {
        b
      }
    }
  }

  println("and attempting to decode it")
  val decoded = decode(output.get)
  println(s"expected $data")
  println(s"got $decoded")
}
