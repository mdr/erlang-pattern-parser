package com.github.mdr.erlangpatternparser

import org.scalatest.Spec
import com.github.mdr.erlangpatternparser.ErlangPatternMatcher._
import com.ericsson.otp.erlang._
import org.scalatest.matchers._
import com.github.mdr.erlangpatternparser.ErlangPatternParser._
import com.github.mdr.erlangpatternparser.OptionExtra._

class PatternSpec extends Spec with ShouldMatchers {

  implicit val recordDefinitions: RecordDefinitions = new RecordDefinitions(List(
    parseRecord(" -record(account, {no, balance, pin, name, transactions = []}). ")))

  describe("The thenSome method") {
    it("should be lazy") {
      def safeDiv(x: Int, y: Int) = (y != 0).thenSome(x / y)
      safeDiv(10, 2) should be(Some(5))
      safeDiv(10, 0) should be(None)
    }
  }

  describe("The pattern parser") {

    it("should parse string prefix patterns") {
      parse(""" "Wibble" ++ Rest """) should be(StringPrefix("Wibble", Variable("Rest")))
      parse(""" "Wibble" ++ Rest """) should be(StringPrefix("Wibble", Variable("Rest")))
    }

    it("should parse record declarations") {
      parseRecord(" -record(account, {no, balance = 0.0, pin, name, transactions = []}). ") should be(
        new RecordDefinition("account", List("no", "balance", "pin", "name", "transactions"),
          Map("transactions" -> ListPattern(List(), None),
            "balance" -> FloatLiteral(0.0))))
    }

  }
  describe("The pattern matcher") {

    it("should match literals") {
      "foo" should matchPattern("foo")
      "foo" should not(matchPattern("bar"))
      "42" should matchPattern("42")
      "42" should not(matchPattern("24"))
      "4.2" should matchPattern("4.2")
      "4.2" should not(matchPattern("2.4"))
      "$a" should matchPattern("$a")
      "$a" should not(matchPattern("$z"))
      "2#0010010" should matchPattern("18")
    }

    it("should let anything match the universal pattern") {
      "{foo, bar, baz}" should matchPattern("_" producing noBindings)
      "{foo, bar, {baz, [1,2,3]}}" should matchPattern("{_, bar, {baz, [1,2,_]}}" producing noBindings)
    }

    it("should match strings") {
      """  "foo" """ should matchPattern(""" "foo" """)
      """  "foo" """ should not(matchPattern(""" "foof" """))
      """  "foof" """ should not(matchPattern(""" "foo" """))
      """  'foo' """ should not(matchPattern(""" "foo" """))
      """  "foo" """ should not(matchPattern(""" 'foo' """))
      """ "foo" """ should not(matchPattern("[102,111,111|foo]"))
      """ "foo" """ should matchPattern("[102,111,111]")
      "[102,111,111]" should matchPattern(""" "foo" """)
      "[102,111,111|foo]" should not(matchPattern(""" "foo" """))
      "[$h, $e, $l, $l, $o]" should matchPattern(""" "hello" """)
    }

    it("should match string prefix patterns") {
      // TODO: Would prefer Rest to be a string, rather than a list of ints
      // """  "WibbleWobble" """  should  matchPattern (""" "Wibble" ++ Rest """ producing bindings("Rest" -> """ "Wobble" """))
      """  "WibbleWobble" """ should matchPattern(""" "Wibble" ++ Rest """ producing bindings("Rest" -> """ [87, 111, 98, 98, 108, 101] """))
    }

    it("should match nested tuples") {
      "{{3, 4}, {5, 6}}" should matchPattern("{{3, 4}, {5, 6}}")
      "{{4, 3}, {6, 5}}" should not(matchPattern("{{3, 4}, {5, 6}}"))
      "{{3, 4}, {5, 6}}" should matchPattern("{{X, 4}, Y}" producing bindings("X" -> "3", "Y" -> "{5, 6}"))
    }

    it("should match mixed lists and tuples") {
      "[1, 2, 3, {4, foo}, [foo]]" should matchPattern("[1,2,3|[{X, Y}, [Y]]]" producing bindings("X" -> "4", "Y" -> "foo"))
      "[1, 2, 3, {4, foo}, [bar]]" should not(matchPattern("[1,2,3|[{X, Y}, [Y]]]"))
    }

    it("should match records") {
      "#account{no = 2234534, pin = 1234, name = 'matt russell'}" should
        matchPattern("#account{pin = Pin, no = Number}" producing bindings("Number" -> "2234534", "Pin" -> "1234"))
      "#account{no = 2234534, pin = 1234, name = 'matt russell'}" should not(matchPattern("#account{pin = 4321}"))
      "#account{no = 2234534, pin = 1234, name = 'matt russell'}" should matchPattern("#account{}")
      "#account{no = 2234534, pin = 1234, name = 'matt russell'}" should matchPattern("{account, _, _, _, _, _}")
    }

    it("should match pattern equations") {
      "{foo, bar}" should matchPattern("X = {foo, Y}" producing bindings("X" -> "{foo, bar}", "Y" -> "bar"))
      "{foo, bar}" should matchPattern("Z = {X, bar} = {foo, Y}" producing bindings("Z" -> "{foo, bar}", "X" -> "foo", "Y" -> "bar"))
      "foo" should matchPattern("(X = X) = (X = X)" producing bindings("X" -> "foo"))
    }

    it("should match lists") {
      "[]" should matchPattern("[]")
      "[1]" should not(matchPattern("[]"))
      "[]" should not(matchPattern("[1]"))
      "[1, 2]" should not(matchPattern("[1, 2, 3]"))
      "[1, 2|foo]" should not(matchPattern("[1, 2]"))
      "[1, 2|foo]" should not(matchPattern("[1, 2, foo]"))
      "[1, 2|foo]" should matchPattern("[1, 2|foo]")
      "[1, 2]" should not(matchPattern("[1, 2|foo]"))
      "[1, 2, 3]" should not(matchPattern("[1, 2]"))
      "[1, 2, 3]" should matchPattern("[1, 2, 3|_]")
      "[1, 2, 3]" should not(matchPattern("[1, 2, 4|_]"))
      "[2, 1]" should not(matchPattern("[1, 2]"))
      "[1, 2, 3, 4]" should matchPattern("[A, 2, B, 4]" producing bindings("A" -> "1", "B" -> "3"))
      "[1, 2, 3, 4]" should matchPattern("[Y, 2|X]" producing bindings("X" -> "[3, 4]", "Y" -> "1"))
      "[1, 2, 3|foo]" should matchPattern("[Y, 2|X]" producing bindings("X" -> "[3|foo]", "Y" -> "1"))
    }

  }

  private def bindings(elems: (String, OtpErlangObject)*) = Map(elems: _*)
  private val noBindings = bindings()

  private def atom(s: String) = new OtpErlangAtom(s)
  private def int(n: Int) = new OtpErlangLong(n)
  private def float(f: Double) = new OtpErlangDouble(f)
  private def tuple(els: OtpErlangObject*) = new OtpErlangTuple(els.toArray)

  implicit def string2WaitingForBindings(s: String): WaitingForBindings = WaitingForBindings(s)

  case class WaitingForBindings(pattern: String) {
    def producing(bindings: Map[String, OtpErlangObject]): PatternAndBindings = PatternAndBindings(pattern, bindings)
  }
  case class PatternAndBindings(pattern: String, expectedBindings: Map[String, OtpErlangObject])

  class ErlangPatternMatcherMatcher(val pattern: String, expectedBindings: Map[String, OtpErlangObject]) extends Matcher[String] {

    def apply(data: String) = {
      val otpObject = new OtpErlangObjectBuilder(recordDefinitions).build(data)
      val resultBindingsOption = new ErlangPatternMatcher(recordDefinitions).matchPattern(pattern, otpObject)
      resultBindingsOption match {
        case Some(resultBindings) ⇒ {
          if (resultBindings == expectedBindings) {
            val negatedFailMessage = data + " matched the pattern " + pattern + " with bindings from expected (expected = " +
              expectedBindings + ", actual = " + resultBindings + ")"
            MatchResult(true, "**ERROR*", negatedFailMessage)
          } else {
            val matched = false
            val failMessage = data + " matched the pattern " + pattern + " with different bindings from expected  (expected = " +
              expectedBindings + ", actual = " + resultBindings + ")"
            MatchResult(false, failMessage, "**ERROR*")
          }
        }
        case None ⇒ {
          val matched = false
          val failMessage = data + " did not match the pattern " + pattern
          MatchResult(false, failMessage, "**ERROR*")
        }
      }
    }
  }

  def matchPattern(pattern: String, bindings: (String, OtpErlangObject)*) = new ErlangPatternMatcherMatcher(pattern, Map(bindings: _*))
  def matchPattern(patternAndBindings: PatternAndBindings) = new ErlangPatternMatcherMatcher(patternAndBindings.pattern, patternAndBindings.expectedBindings)

}
