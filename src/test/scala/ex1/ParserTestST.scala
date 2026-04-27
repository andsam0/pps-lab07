package ex1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.should

class BasicParserTest extends AnyFlatSpec:
  val parser = new BasicParser(Set('a', 'b', 'c'))

  "A BasicParser" should "correctly parse elements" in:
    parser.parseAll("aabc".toList) should be (true)

  "A BasicParser" should "not correctly parse elements" in :
    parser.parseAll("aabcdc".toList) should be (false)

  "A BasicParser" should "correctly parse empty string" in :
    parser.parseAll("".toList) should be (true)

class NotEmptyParserTest extends AnyFlatSpec:
  def parserNE = new NonEmptyParser(Set('0', '1'))
  "A NotEmptyParser" should "correctly parse elements" in:
    parserNE.parseAll("0101".toList) should be (true)

  "A NotEmptyParser" should "not correctly parse elements" in :
    parserNE.parseAll("0123".toList) should be (false)

  "A NotEmptyParser" should "not correctly parse empty string" in :
    parserNE.parseAll(List()) should be (false)