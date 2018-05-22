package space.perpetual

import org.scalatest._

class Base58Spec extends FlatSpec with Matchers {
  "encode" should "encode a string" in {
    Base58.encode("1") should be ("r")
    Base58.encode("12") should be ("4k9")
    Base58.encode("1918239081290389120839012") should be ("Lomxj63p575TnvuvW4VkMz6AgZTAkL5uc5")
  }

  it should "throw an exception when the input is an empty string" in {
    a [IllegalArgumentException] should be thrownBy {
      Base58.encode("")
    }
  }

  "decode" should "decode a base 58 string" in {
    Base58.decode("r") should be ("1")
    Base58.decode("4k9") should be ("12")
    Base58.decode("Lomxj63p575TnvuvW4VkMz6AgZTAkL5uc5") should be ("1918239081290389120839012")
  }

  it should "throw an exception when the input contains characters not in the alphabet" in {
    a [IllegalArgumentException] should be thrownBy {
      Base58.decode("çã[]")
    }
  }

  it should "throw an exception when the input is empty" in {
    a [IllegalArgumentException] should be thrownBy {
      Base58.decode("")
    }
  }
}
