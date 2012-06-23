package com.github.mdr.erlangpatternparser

import com.github.mdr.erlangpatternparser.OptionExtra._

class RecordDefinitions(val recordDefinitions: List[RecordDefinition]) {
  private val recordDefinitionsMap: Map[String, RecordDefinition] = {
    var result: Map[String, RecordDefinition] = Map()
    for (defn @ RecordDefinition(name, _, _) ← recordDefinitions)
      result = result + (name -> defn)
    result
  }
  def this() = this(List())

  def get(recordName: String) = recordDefinitionsMap get recordName
  def contains(recordName: String) = recordDefinitionsMap contains recordName
  def apply(recordName: String) = recordDefinitionsMap(recordName)
}

case class RecordDefinition(val recordName: String, val fields: List[String], val defaults: Map[String, ErlangPattern]) {
  if (fields.distinct != fields)
    throw new IllegalArgumentException("Cannot have duplicate fields in record " + recordName + ": " + fields)

  def size = fields.size
  def tupleIndex(field: String) =
    for (index ← maybeNegativeToOption(fields.indexOf(field)))
      yield index + 1

}
