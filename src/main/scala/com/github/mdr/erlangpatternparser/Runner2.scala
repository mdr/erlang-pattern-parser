package com.github.mdr.erlangpatternparser

import com.ericsson.otp.erlang._

object Runner2 {

  def main(args: Array[String]): Unit = {
    val parser = new ErlangPatternParser
    val result = parser.parseAll(parser.pattern, """{
            "wibble " "wobble " "w oo o o",
            1.0,
            -5.1e6,
            5.1e+6,
            0.346E-20,
            300,
            -12,
            000001,
            2#0010010,
            -8#377,
            16#FA66,
            16#fa66,
            {"foo", "bar, baz", "bizzle"}, 
            #person{name = Bob, foo = {Bar, Baz}},  
              foo,[],
              [A, B,C, {_, {}}], 
              [H|T],
             [1, 2, 3 | foo],
            {_, _},
            Bar, _foo, _F, {'baz', '', 'buzz \' \t bozz', bizzle}, {my_donkey}, {}, {bob@localhost, some_more}
        }""").get
    println(result)

    val recordDefn = new RecordDefinition("account", List("no", "balance", "pin", "name", "transactions"), Map())
    val recordDefinitions = new RecordDefinitions(List(recordDefn))
    val patternMatcher = new ErlangPatternMatcher(recordDefinitions)
    val objectBuilder = new OtpErlangObjectBuilder(recordDefinitions)
    val obj = objectBuilder.build("""#account{no = Number, pin = Pin}""", Map("Number" -> "58323423480", "Pin" -> "1234"))
    val patternMatchResult = patternMatcher.matchPattern("""#account{no = Number, pin = Pin}""", obj, Map())
    println("Object = " + obj)
    println("Pattern match result = " + patternMatchResult)
    None
    /*        
        val node = new OtpNode("jvm")
        val mbox = node.createMbox("jvm_mailbox")
         if (node.ping("foo", 2000)) 
          println("remote is up")
        else 
          println("remote is not up")
        val msg = new Array[OtpErlangObject](2)
        msg(0) = mbox.self()
        msg(1) = new OtpErlangAtom("hello, world")
        val tuple = new OtpErlangTuple(msg);    

        mbox.send("echo", "foo@matt-desktop", tuple);    

        while (true) {
            val reply = mbox.receive(30000)
            println("Reply: " + reply)
            if (reply != null)
            	println("Reply type: " + reply.getClass)
//            reply match {
//              case list: OtpErlangList => println("arity = " + list.arity + ", els = " + (List() ++ list.elements) + ", tail= " + list.getLastTail + ", 1st tail= " + list.getNthTail(1)+ ", 2nd tail = " + list.getNthTail(2))
//              case _ => 
//            }
            
            val result2 = patternMatcher.matchPattern("#account{no = Number, pin = Pin}", reply, Map[String, OtpErlangObject]())
            println("Pattern match result = " + result2)
        }
    
  */
  }

}
