package com.github.mdr.erlangpatternparser

import com.ericsson.otp.erlang._
import com.github.mdr.erlangpatternparser.OptionExtra._

class MatchableErlangObject(otpObject: OtpErlangObject, recordDefinitions: RecordDefinitions) {
  def this(otpObject: OtpErlangObject) = this(otpObject, new RecordDefinitions())
  type Bindings = Map[String, OtpErlangObject]
  def matchPattern(patternString: String): Option[Bindings] = new ErlangPatternMatcher(recordDefinitions).matchPattern(patternString, otpObject)
}

object ErlangPatternMatcher {
  implicit def otpErlangObject2MatchableErlangObject(otpObject: OtpErlangObject)(implicit recordDefinitions: RecordDefinitions): MatchableErlangObject = {
    new MatchableErlangObject(otpObject, recordDefinitions)
  }
  implicit def string2MatchableErlangObject(s: String)(implicit recordDefinitions: RecordDefinitions): MatchableErlangObject = {
    new MatchableErlangObject(new OtpErlangObjectBuilder(recordDefinitions).build(s), recordDefinitions)
  }
  implicit def string2OtpErlangObject(s: String)(implicit recordDefinitions: RecordDefinitions): OtpErlangObject = {
    new OtpErlangObjectBuilder(recordDefinitions).build(s)
  }
}

class ErlangPatternMatcher(private val recordDefinitions: RecordDefinitions) { matcher ⇒

  def this() = this(new RecordDefinitions())

  type Bindings = Map[String, OtpErlangObject]

  def matchPattern(patternString: String, otpObject: OtpErlangObject): Option[Bindings] = matchPattern(patternString, otpObject, Map())

  def matchPattern(patternString: String, otpObject: OtpErlangObject, bindings: Bindings): Option[Bindings] = {
    // TODO: Scan pattern for record patterns and check against record name and field names against stored definitions
    val pattern: ErlangPattern = ErlangPatternParser.parse(patternString)
    doPatternMatch(pattern, otpObject, bindings)
  }

  private def doPatternMatch(pattern: ErlangPattern, otpObject: OtpErlangObject, bindings: Bindings): Option[Bindings] = {
    pattern match {
      case Atom(atom)                          ⇒ matchAtom(atom, otpObject, bindings)
      case Tuple(members)                      ⇒ matchTuple(members, otpObject, bindings)
      case Variable(variableName)              ⇒ matchVariable(variableName, otpObject, bindings)
      case UniversalPattern                    ⇒ Some(bindings)
      case ListPattern(members, tailOption)    ⇒ matchList(members, tailOption, otpObject, bindings)
      case IntegerLiteral(n)                   ⇒ matchInteger(n, otpObject, bindings)
      case FloatLiteral(n)                     ⇒ matchFloat(n, otpObject, bindings)
      case CharacterLiteral(c)                 ⇒ matchCharacter(c, otpObject, bindings)
      case StringLiteral(s)                    ⇒ matchString(s, otpObject, bindings)
      case Record(recordName, fieldPatterns)   ⇒ matchRecord(recordName, fieldPatterns, otpObject, bindings)
      case PatternEquation(pattern1, pattern2) ⇒ matchPatternEquation(pattern1, pattern2, otpObject, bindings)
      case StringPrefix(prefix, pattern)       ⇒ matchStringPrefix(prefix, pattern, otpObject, bindings)
    }
  }

  private def matchStringPrefix(prefix: String, pattern: ErlangPattern, otpObject: OtpErlangObject, bindings: Bindings) =
    doPatternMatch(stringPatternAsListPattern(prefix, Some(pattern)), otpObject, bindings)

  private def matchPatternEquation(pattern1: ErlangPattern, pattern2: ErlangPattern, otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      bindings1 ← doPatternMatch(pattern1, otpObject, bindings)
      bindings2 ← doPatternMatch(pattern2, otpObject, bindings1)
    } yield bindings2

  private def matchRecord(recordName: String, fieldPatterns: List[(String, ErlangPattern)], otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      otpTuple ← safeCast(otpObject, classOf[OtpErlangTuple])
      recordDefinition ← recordDefinitions.get(recordName)
      tupleElements = otpTuple.elements()
      if otpTuple.arity > 0
      firstElement = tupleElements(0)
      newBindings ← matchAtom(recordName, tupleElements(0), bindings) // newBindings == bindings
      result ← foldM(doRecordPatternMatch(recordDefinition, tupleElements), newBindings, fieldPatterns)
    } yield result

  private def doRecordPatternMatch(recordDefinition: RecordDefinition, elements: Array[OtpErlangObject]) =
    (bindings: Bindings, fieldNameAndPattern: (String, ErlangPattern)) ⇒
      fieldNameAndPattern match {
        case (fieldName, pattern) ⇒
          for {
            index ← recordDefinition.tupleIndex(fieldName)
            otpElement = elements(index)
            result ← matcher.doPatternMatch(pattern, otpElement, bindings)
          } yield result
      }

  private def matchString(s: String, otpObject: OtpErlangObject, bindings: Bindings) =
    otpObject match {
      case otpString: OtpErlangString ⇒ (otpString.stringValue == s) thenSome bindings
      case otpList: OtpErlangList     ⇒ doPatternMatch(stringPatternAsListPattern(s), otpObject, bindings)
      case _                          ⇒ None
    }

  private def stringPatternAsListPattern(s: String, tailPatternOption: Option[ErlangPattern]) = {
    val integerLiteralsToMatch = for (c ← List() ++ s)
      yield IntegerLiteral(c.toInt)
    ListPattern(integerLiteralsToMatch, tailPatternOption)
  }

  private def stringPatternAsListPattern(s: String): ListPattern = stringPatternAsListPattern(s, None)

  private def matchCharacter(c: Char, otpObject: OtpErlangObject, bindings: Bindings) = matchInteger(c.toInt, otpObject, bindings)

  private def matchFloat(n: Double, otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      otpFloat ← safeCast(otpObject, classOf[OtpErlangDouble])
      if (otpFloat.doubleValue == n)
    } yield bindings

  private def matchInteger(n: BigInt, otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      otpLong ← safeCast(otpObject, classOf[OtpErlangLong])
      if otpLong.bigIntegerValue == n.bigInteger
    } yield bindings

  private def matchMembers(bindings: Bindings, members: List[ErlangPattern], otpElements: Array[OtpErlangObject]) = {
    def doPatternMatch(bindings: Bindings, pair: (ErlangPattern, OtpErlangObject)): Option[Bindings] =
      pair match { case (pattern, member) ⇒ matcher.doPatternMatch(pattern, member, bindings) }
    foldM(doPatternMatch, bindings, members.zip(List() ++ otpElements))
  }

  private def matchList(members: List[ErlangPattern], tailOption: Option[ErlangPattern], otpObject: OtpErlangObject, bindings: Bindings): Option[Bindings] = {
    otpObject match {
      case otpString: OtpErlangString ⇒ matchList(members, tailOption, new OtpErlangList(otpString.stringValue), bindings)

      case otpList: OtpErlangList ⇒
        (otpList.arity >= members.size) then {
          tailOption match {
            case None ⇒
              if (otpList.getLastTail == null)
                (otpList.arity == members.size) then matchMembers(bindings, members, otpList.elements)
              else
                None
            case Some(tailPattern) ⇒
              for {
                tail ← nullableToOption(otpList.getNthTail(members.size))
                memberBindings ← matchMembers(bindings, members, otpList.elements)
                allBindings ← doPatternMatch(tailPattern, tail, memberBindings)
              } yield allBindings
          }
        }
      case _ ⇒ None
    }
  }

  private def matchVariable(variableName: String, otpObject: OtpErlangObject, bindings: Bindings) =
    bindings.get(variableName) match {
      case Some(priorBoundOtpObject) ⇒ (otpObject == priorBoundOtpObject) thenSome bindings
      case None                      ⇒ Some(bindings + (variableName -> otpObject))
    }

  private def matchTuple(members: List[ErlangPattern], otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      otpTuple ← safeCast(otpObject, classOf[OtpErlangTuple])
      if otpTuple.arity() == members.size
      result ← matchMembers(bindings, members, otpTuple.elements)
    } yield result

  private def matchAtom(atom: String, otpObject: OtpErlangObject, bindings: Bindings) =
    for {
      otpAtom ← safeCast(otpObject, classOf[OtpErlangAtom])
      if atom == otpAtom.atomValue
    } yield bindings

}
