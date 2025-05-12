import scala.annotation.tailrec

object RunLengthEncoding:
   def encode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String = cs match
         case Nil => acc
         case _   =>
            val (first, rest) = cs.span(_ == cs.head)
            loop(rest, acc + (if first.length > 1 then first.length.toString else "") + first.head)
      loop(s.toList, "")

   def decode(s: String): String =
      @tailrec
      def loop(cs: List[Char], acc: String): String = cs match
         case Nil => acc
         case _   =>
            val (digits, rest) = cs.span(_.isDigit)
            loop(rest.drop(1), acc + rest.head.toString * (if digits.isEmpty then 1 else digits.mkString.toInt))
      loop(s.toList, "")
