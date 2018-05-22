package space.perpetual

import scala.annotation.tailrec

object Base58 {
  val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val base = 58

  def encode(str: String): String = {
    if (str.isEmpty)
      throw new IllegalArgumentException("The input string is empty.")

    @tailrec
    def encode1(n: BigInt, buf: String): String = {
      if (n < 1) buf
      else encode1(n / base, alphabet((n % base).toInt) + buf)
    }

    // Generate a huge integer from the string representation.
    val decimal = str
      .map(_.toByte.toInt)
      .foldLeft(BigInt(0)){ (acc: BigInt, byte: Int) => (acc * 256) + byte }

    encode1(decimal, "").mkString
  }

  def decode(str: String): String = {
    if (str.isEmpty || str.exists(chr => (alphabet.contains(chr) == false)))
      throw new IllegalArgumentException("The input string is not a valid base 58 string.")

    // Transform the base 58 string back into a huge integer.
    val decimal = str
      .foldLeft(BigInt(0)){ (acc: BigInt, chr: Char) => (acc * base) + alphabet.indexOf(chr) }

    def decode1(remainder: BigInt): List[Int] = {
      if (remainder <= 0) Nil
      else (remainder % 256).toInt :: decode1((remainder: BigInt) / 256)
    }

    decode1(decimal).reverse.map(_.toChar).mkString
  }
}
