import scopt._

import scala.collection.immutable.BitSet
import scala.io.StdIn

object Main {

  case class Config(mode: String = "", value: Int = 0)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("stegosaurus") {
      head("stegosaurus", "0.1")

     cmd("encode").action( (_, c) => c.copy(mode = "encode") ).
        text("encode the data being sent in with the value").
        children(
          arg[Int]("value").text("the number to hide in the text").action((i, c) => c.copy(value = i))
        )
     cmd("decode").action( (_, c) => c.copy(mode = "decode") ).
        text("decode the data being sent in")
    }

    // parser.parse returns Option[C]
    parser.parse(args, Config()) match {
      case Some(config) =>
        config.mode match {
          case "encode" =>
            println(s"encoding the value ${config.value} into each line if I can")
            var ok = true
            while (ok) {
              val line = StdIn.readLine()
              ok = line != null
              if (ok) {
                val encoded = Stegosaurus.encode(line, BitSet.fromBitMask(Array(config.value)))
                encoded match {
                  case Some(o) => println(s"encoded: $o")
                  case None => println("failed to encode")
                }
              }
            }
          case "decode" =>
            var ok = true
            while (ok) {
              val line = StdIn.readLine()
              ok = line != null
              if (ok) {
                val result = Stegosaurus.decode(line)
                println(s"I got the number ${result}")
              }
            }
          case _ =>
            println("please select a mode")
        }
      case None =>
      // arguments are bad, error message will have been displayed
    }
  }
}
