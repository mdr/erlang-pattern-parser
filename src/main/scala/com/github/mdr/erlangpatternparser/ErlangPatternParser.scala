package com.github.mdr.erlangpatternparser
import scala.util.parsing.combinator._

object ErlangPatternParser {
  def parse(s: String): ErlangPattern = new ErlangPatternParser().parse(s)

  def parseRecord(s: String) = new ErlangPatternParser().parseRecord(s)

}
// Page references refer to the page number in Erlang spec, offset +22 pages from the printed numbers in the document
class ErlangPatternParser extends RegexParsers {

  def parse(s: String): ErlangPattern = parseAll(pattern, s).get

  def parseRecord(s: String) = parseAll(recordDeclaration, s).get

  // TODO: Expressions in patterns: {?THRESHOLD + 1, ok}
  // TODO: Bit syntax expressions
  // TODO: Comments
  // : Parser[RecordDefinition]
  def recordDeclaration = // p. 144
    ("-record" ~ "(") ~> atomLiteral ~ (("," ~ "{") ~> repsep(recordFieldDeclaration, ",") <~ ("}" ~ ")" ~ ".")) ^^ {
      case Atom(recordName) ~ fields ⇒
        var fieldNames: List[String] = List()
        var defaults: Map[String, ErlangPattern] = Map()
        for ((Atom(fieldName), defaultOption) ← fields) {
          fieldNames = fieldNames ::: List(fieldName)
          for (default ← defaultOption)
            defaults = defaults + (fieldName -> default)
        }
        new RecordDefinition(recordName, fieldNames, defaults)
    }

  def recordFieldDeclaration = atomLiteral ~ ("=" ~> initialFieldValue).? ^^ { case atom ~ initialFieldOption ⇒ (atom, initialFieldOption) } // p. 106

  def initialFieldValue = pattern

  def pattern: Parser[ErlangPattern] = ( // p. 85 
    stringPrefix ~ patternRest | // TODO: Why does this break if not first?
    atomicLiteral ~ patternRest |
    variable ~ patternRest |
    universalPattern ~ patternRest |
    tuple ~ patternRest |
    record ~ patternRest |
    list ~ patternRest |
    ("(" ~> pattern <~ ")") ~ patternRest) ^^ patternEquationOrNot

  def patternRest: Parser[Option[ErlangPattern]] = (("=" ~> pattern) ~ patternRest ^^ patternEquationOrNot).?

  def stringPrefix = (stringLiterals <~ "++") ~ pattern ^^ { case (StringLiteral(s) ~ p) ⇒ StringPrefix(s, p) }

  def patternEquationOrNot(patternAndPatternOption: ~[ErlangPattern, Option[ErlangPattern]]): ErlangPattern = patternAndPatternOption match {
    case (p1 ~ Some(p2)) ⇒ PatternEquation(p1, p2)
    case (p1 ~ None)     ⇒ p1
  }

  def atomicLiteral = // p. 112 
    floatLiteral | // TODO: Why does floatLiteral have to come first? 
      integerLiteral |
      charLiteral |
      stringLiterals |
      atomLiteral

  def floatLiteral = """[+-]?[0-9]+\.[0-9]+([Ee][+-]?[0-9]+)?""".r ^^ { f ⇒ FloatLiteral(java.lang.Double.parseDouble(f)) } // p. 49

  def stringLiterals = stringLiteral.+ ^^ { s ⇒ StringLiteral(s.mkString("", "", "")) }

  def stringLiteral = "\"" ~> """[^"]*""".r <~ "\"" // p. 50, TODO: escapes

  def charLiteral = "$" ~> """[^\\]""".r ^^ { s ⇒ CharacterLiteral(s.charAt(0)) } // p. 49-50, TODO: escapes p. 47

  def decimalLiteral = "[0-9]+".r ^^ Integer.parseInt // p. 47 - 48
  def explicitRadixLiteral = // TODO: Parameterised parser?
    "2#" ~> "[0-1]+".r ^^ { Integer.parseInt(_, 2) } |
      "3#" ~> "[0-2]+".r ^^ { Integer.parseInt(_, 3) } |
      "4#" ~> "[0-3]+".r ^^ { Integer.parseInt(_, 4) } |
      "5#" ~> "[0-4]+".r ^^ { Integer.parseInt(_, 5) } |
      "6#" ~> "[0-5]+".r ^^ { Integer.parseInt(_, 6) } |
      "7#" ~> "[0-6]+".r ^^ { Integer.parseInt(_, 7) } |
      "8#" ~> "[0-7]+".r ^^ { Integer.parseInt(_, 8) } |
      "9#" ~> "[0-8]+".r ^^ { Integer.parseInt(_, 9) } |
      "10#" ~> "[0-9]+".r ^^ { Integer.parseInt(_, 10) } |
      "11#" ~> "[0-9a-aA-A]+".r ^^ { Integer.parseInt(_, 11) } |
      "12#" ~> "[0-9a-bA-B]+".r ^^ { Integer.parseInt(_, 12) } |
      "13#" ~> "[0-9a-cA-C]+".r ^^ { Integer.parseInt(_, 13) } |
      "14#" ~> "[0-9a-dA-D]+".r ^^ { Integer.parseInt(_, 14) } |
      "15#" ~> "[0-9a-eA-E]+".r ^^ { Integer.parseInt(_, 15) } |
      "16#" ~> "[0-9a-fA-F]+".r ^^ { Integer.parseInt(_, 16) }

  def integerLiteral = (("+" | "-").? ~ (explicitRadixLiteral | decimalLiteral)) ^^ {
    case Some("-") ~ x ⇒ IntegerLiteral(-x)
    case Some("+") ~ x ⇒ IntegerLiteral(x)
    case None ~ x      ⇒ IntegerLiteral(x)
  }

  private val NAME_CHAR = "[a-zA-Z0-9@_]" // p. 51

  def atomLiteral = quotedAtomLiteral | unquotedAtomLiteral
  def unquotedAtomLiteral = ("[a-z]" + NAME_CHAR + "*").r ^^ Atom // p. 50, TODO: Check reserved words
  def quotedAtomLiteral = "'" ~> """(\\'|[^''])*""".r <~ "'" ^^ dequoteAtom // p. 51, 44
  private def dequoteAtom(s: String) = { // TODO: Other atom escapes
    Atom(s.replace("""\'""", "'"))
  }

  def variable = ("[A-Z]" + NAME_CHAR + "*").r ^^ Variable | ("_" + NAME_CHAR + "+").r ^^ Variable // p. 52

  def universalPattern = "_" ^^^ UniversalPattern // p. 52

  def record = "#" ~> atomLiteral ~ ("{" ~> repsep(recordField, ",") <~ "}") ^^ { case (Atom(recordName) ~ fieldPatterns) ⇒ Record(recordName, fieldPatterns) } // p. 86

  def recordField = atomLiteral ~ ("=" ~> pattern) ^^ { case (Atom(fieldName) ~ p) ⇒ (fieldName, p) }

  def tuple = "{" ~> repsep(pattern, ",") <~ "}" ^^ Tuple // p. 113

  def list = "[" ~> repsep(pattern, ",") ~ ("|" ~> pattern).? <~ "]" ^^ { case (members ~ tail) ⇒ ListPattern(members, tail) } // p. 113-114

}

