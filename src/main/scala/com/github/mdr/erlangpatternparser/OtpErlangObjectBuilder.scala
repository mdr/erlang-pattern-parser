package com.github.mdr.erlangpatternparser

import com.ericsson.otp.erlang._
import scala.collection.JavaConversions._

class OtpErlangObjectBuilder(private val recordDefinitions: RecordDefinitions) {

  def this() = this(new RecordDefinitions())

  type Bindings = Map[String, Any]

  def build(patternString: String): OtpErlangObject = build(patternString, Map())
  def build(patternString: String, bindings: Bindings): OtpErlangObject = {
    val pattern: ErlangPattern = ErlangPatternParser.parse(patternString)
    // TODO: Check pattern doesn't contain the universal pattern
    doBuild(pattern, bindings)
  }

  private def doBuild(pattern: ErlangPattern, bindings: Bindings): OtpErlangObject = {
    pattern match {
      case Atom(atom)                        ⇒ buildAtom(atom)
      case Tuple(members)                    ⇒ buildTuple(members, bindings)
      case Variable(variableName)            ⇒ buildVariable(variableName, bindings)
      case ListPattern(members, tailOption)  ⇒ buildList(members, tailOption, bindings)
      case IntegerLiteral(n)                 ⇒ buildInteger(n)
      case FloatLiteral(n)                   ⇒ buildFloat(n)
      case CharacterLiteral(c)               ⇒ buildCharacter(c)
      case StringLiteral(s)                  ⇒ buildString(s)
      case Record(recordName, fieldPatterns) ⇒ buildRecord(recordName, fieldPatterns, bindings)
      case StringPrefix(s, pattern)          ⇒ buildStringWithPrefix(s, pattern, bindings)
      case UniversalPattern                  ⇒ throw new AssertionError("Cannot build an object for the universal pattern _")
      case PatternEquation(p1, p2)           ⇒ throw new AssertionError("Cannot build an object for a pattern equation: " + p1 + ", " + p2)

    }
  }

  private def buildInteger(n: BigInt) = new OtpErlangLong(n.bigInteger)
  private def buildFloat(n: Double) = new OtpErlangDouble(n)
  private def buildCharacter(c: Char) = new OtpErlangChar(c)
  private def buildString(s: String) = new OtpErlangString(s)

  private def buildStringWithPrefix(s: String, pattern: ErlangPattern, bindings: Bindings) = {
    new OtpErlangList(new OtpErlangList(s).elements, doBuild(pattern, bindings))
  }

  private def buildVariable(variable: String, bindings: Bindings): OtpErlangObject = {
    if (bindings contains variable) {
      val value = bindings(variable)
      makeOtpObject(value)
    } else
      throw new IllegalArgumentException("No binding for variable " + variable + " in " + bindings)
  }

  private def makeOtpObject(value: Any): OtpErlangObject = {
    value match {
      case otpObject: OtpErlangObject ⇒ otpObject
      case n: Int                     ⇒ new OtpErlangInt(n.intValue)
      case n: java.lang.Integer       ⇒ new OtpErlangInt(n.intValue)
      case n: java.math.BigInteger    ⇒ new OtpErlangLong(n)
      case n: BigInt                  ⇒ new OtpErlangLong(n.bigInteger)
      case n: Long                    ⇒ new OtpErlangLong(n.longValue)
      case n: java.lang.Long          ⇒ new OtpErlangLong(n.longValue)
      // TODO: shorts etc
      case s: String                  ⇒ new OtpErlangString(s)
      // TODO: Java arrays
      case xs: Iterable[_]            ⇒ new OtpErlangList(xs.map(makeOtpObject).toArray)
      // case xs: java.lang.Iterable[_] => new OtpErlangList(new Array(0) ++ xs.map(makeOtpObject))
      case _                          ⇒ throw new IllegalArgumentException("Cannot create an otp object from " + value)
    }
  }

  private def buildAtom(atom: String) = new OtpErlangAtom(atom) // TODO: Escape bad chars 

  private def buildTuple(members: List[ErlangPattern], bindings: Bindings) = {
    val elements = members.map(doBuild(_, bindings)).toArray
    new OtpErlangTuple(elements)
  }

  private def buildList(members: List[ErlangPattern], tailOption: Option[ErlangPattern], bindings: Bindings) = {
    val elements = members.map(doBuild(_, bindings)).toArray
    tailOption match {
      case None       ⇒ new OtpErlangList(elements)
      case Some(tail) ⇒ new OtpErlangList(elements, doBuild(tail, bindings))
    }
  }
  private def buildRecord(recordName: String, fieldPatterns: List[(String, ErlangPattern)], bindings: Bindings) = {
    val fieldPatternsMap = Map(fieldPatterns: _*)
    if (recordDefinitions contains recordName) {
      val recordDefinition = recordDefinitions(recordName)
      val tupleElements = new Array[OtpErlangObject](recordDefinition.size + 1)
      tupleElements(0) = buildAtom(recordName)
      for (fieldName ← recordDefinition.fields) {
        val tupleElement = if (fieldPatternsMap contains fieldName)
          doBuild(fieldPatternsMap(fieldName), bindings)
        else // TODO: build default
          buildAtom("undefined")
        val tupleIndex = recordDefinition.tupleIndex(fieldName).getOrElse(-1) // TODO
        tupleElements(tupleIndex) = tupleElement
      }
      new OtpErlangTuple(tupleElements)
    } else
      throw new IllegalArgumentException("No definition available for record " + recordName)
  }
}
