package com.github.mdr.erlangpatternparser

abstract sealed trait ErlangPattern
case class Atom(atom: String) extends ErlangPattern
case class Variable(variable: String) extends ErlangPattern
case object UniversalPattern extends ErlangPattern
case class Tuple(members: List[ErlangPattern]) extends ErlangPattern
case class ListPattern(members: List[ErlangPattern], tail: Option[ErlangPattern]) extends ErlangPattern
case class Record(recordName: String, fieldPatterns: List[(String, ErlangPattern)]) extends ErlangPattern // TODO: Duplicate field names not allowed, maybe use (ordered) Map
case class IntegerLiteral(i: BigInt) extends ErlangPattern
case class CharacterLiteral(i: Char) extends ErlangPattern
case class StringLiteral(s: String) extends ErlangPattern
case class FloatLiteral(d: Double) extends ErlangPattern
case class PatternEquation(pattern1: ErlangPattern, pattern2: ErlangPattern) extends ErlangPattern
case class StringPrefix(s: String, pattern: ErlangPattern) extends ErlangPattern