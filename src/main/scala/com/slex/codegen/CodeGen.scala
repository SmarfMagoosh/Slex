  package com.slex.codegen

  import com.slex.parsers.SlexAtom.*
  import com.slex.parsers.{SlexAtom, SlexParser}

  import java.io.{File, PrintWriter}

  object CodeGen {
    def generate(slexFile: String): Unit = {
      val ast: SlexAtom.LANG = SlexParser(new File(slexFile)) match
        case SlexParser.Success(tree, _) => tree
        case SlexParser.Failure(msg, _) => throw new Exception(msg)
        case SlexParser.Error(msg, _) => throw new Exception(msg)

      val langName: String = ast.header.name.text

      val dfaFile = new File(s"./${langName}DFAs.txt")
      val tokenTypeFile = new File(s"./src/main/scala/parsing/${langName}TokenType.scala")
      val lexerFile = new File(s"./src/main/scala/parsing/${langName}Lexer.scala")

      dfaFile.createNewFile()
      tokenTypeFile.createNewFile()
      lexerFile.createNewFile()

      val dfaWriter = new PrintWriter(dfaFile)
      val lexerWriter = new PrintWriter(lexerFile)
      val tokenTypeWriter = new PrintWriter(tokenTypeFile)

      tokenTypeWriter.write(s"enum ${langName}TokenType:\n")
      tokenTypeWriter.write("\tcase IGNORE\n")

      // generate tokens and DFAs for keywords
      ast.header.keywords.foreach({ token =>
          val t = token.text
          val dfa = AutomatonFactory.keywordDFA(t.toLowerCase)
          dfaWriter.write(dfa.serialize(t))
          tokenTypeWriter.write(s"\tcase $t\n")
      })

      // generate tokens and DFAs for valid punctuation
      ast.header.punctuation.foreach({ token =>
        val t = token.text
        val kw = Punctuation.aliasToPunc(t)
        val dfa = AutomatonFactory.keywordDFA(kw)
        dfaWriter.write(dfa.serialize(t))
        tokenTypeWriter.write(s"\tcase $t\n")
      })

      // generate tokens and DFAs for regex rules
      val numRules = ast.rules.rules.length - 1
      ast.rules.rules.zipWithIndex.foreach({ case (RULE(regex, TOKEN(token)), i) =>
        val dfa = AutomatonFactory.constructDFA(regex)
        dfaWriter.write(dfa.serialize(token))
        if token != "IGNORE" then tokenTypeWriter.write(s"\tcase $token\n")
      })

      // codegen footers
      tokenTypeWriter.write(s"end ${langName}TokenType")

      // generate token file
      val f1 = new File("./src/main/scala/parsing/Token.scala")
      f1.createNewFile()

      val pw1 = new PrintWriter(f1)
      pw1.write(
        s"case class Token(tokenType: ${langName}TokenType, lexeme: String, lineNum: Int, linePos: Int)\n"
      )

      // generate lexer file
      val f2 = new File(s"./src/main/scala/parsing/${langName}Lexer.scala")
      f2.createNewFile()

      val pw2 = new PrintWriter(f2)
      pw2.write(
        s"""
          |import com.slex.automatons.DFA
          |import scala.annotation.tailrec
          |import java.io.File
          |import java.util.Scanner
          |
          |class ${langName}Lexer {
          |  private val automatons: List[(${langName}TokenType, DFA)] = {
          |    val sb = new StringBuilder()
          |    val scnr = new Scanner(new File("${langName}DFAs.txt"))
          |    scnr.useDelimiter(",")
          |
          |    def parse(): List[(${langName}TokenType, DFA)] = {
          |      if !scnr.hasNext() then Nil else {
          |        val token = scnr.next()
          |        val start = scnr.nextInt()
          |        val accepting = (0 until start).map(_ => scnr.nextInt()).toSet
          |        val states = scnr.nextInt()
          |        val tfun: Array[Map[Int, Int]] = (0 until states).map(_ => {
          |          val numTrans = scnr.nextInt()
          |          if numTrans == 0 then Map() else {
          |            (0 until numTrans).map(_ => {
          |              scnr.nextInt() -> scnr.nextInt()
          |            }).toMap
          |          }
          |        }).toArray
          |        (${langName}TokenType.valueOf(token), DFA(start, accepting, tfun)) :: parse()
          |      }
          |    }
          |    val res = parse()
          |    println(res)
          |    scnr.close()
          |    res
          |  }
          |
          |  private var components: List[(${langName}TokenType, DFA)] = automatons
          |  private var lineNum = 1
          |  private var linePos = 1
          |
          |  def apply(s: String) = {
          |    val ret = lex(s)
          |    reset()
          |    ret
          |  }
          |
          |  def apply(f: File): List[Token] = {
          |    val scnr = new Scanner(f)
          |    val sb = new StringBuilder()
          |    while scnr.hasNextLine do {
          |      sb.append(scnr.nextLine() + "\\n")
          |    }
          |    val ret = lex(sb.toString())
          |    reset()
          |    ret
          |  }
          |
          |  /**
          |   * resets the linenum and linepos variable so another file can be lexed
          |   */
          |  private def reset(): Unit = {
          |    lineNum = 1
          |    linePos = 1
          |    components = automatons
          |    components.foreach(_._2.reset())
          |  }
          |
          |  private def posUpdate(input: Char): Unit = {
          |    if input == '\\n' then {
          |      linePos = 1
          |      lineNum += 1
          |    }  else {
          |      linePos += 1
          |    }
          |  }
          |
          |  @tailrec private def acceptToken(comps: List[(${langName}TokenType, DFA)]): ${langName}TokenType = comps match
          |    case (ty, m)::t => if m.isAccepting then ty else acceptToken(t)
          |    case _ => ${langName}TokenType.IGNORE
          |
          |  private def lex(input: String): List[Token] = {
          |    if input.isEmpty then Nil else {
          |      val (sln, slp) = (lineNum, linePos)
          |      var current_pos = 0
          |
          |      // all of these variables are used so we can jump backwards to that last state in which we were accepting
          |      var acceptPos = 0
          |      var accLp = 0
          |      var accLn = 0
          |      var accTok = acceptToken(components)
          |
          |      // while we have a non-trapped component
          |      while components.nonEmpty && current_pos < input.length do {
          |        // have each DFA process the next char
          |        val c = input.charAt(current_pos)
          |        posUpdate(c)
          |        components = transition(components, c)
          |
          |        // save accepting information if applicable
          |        current_pos += 1
          |        if components.exists(_._2.isAccepting) then {
          |          acceptPos = current_pos
          |          accLp = linePos
          |          accLn = lineNum
          |          accTok = acceptToken(components)
          |        }
          |      }
          |
          |      // jump back to last accept state
          |      val (lexeme, rem) = input.splitAt(acceptPos)
          |      components = automatons
          |      components.foreach(_._2.reset())
          |      lineNum = accLn
          |      linePos = accLp
          |
          |      if lexeme.isEmpty then {
          |        throw new Exception(s"lexer could not match any characters. Remaining input:\\n \\t\\\"$$input\\\"")
          |      }
          |
          |      accTok match
          |        case ${langName}TokenType.IGNORE => lex(rem)
          |        case _ => Token(accTok, lexeme, sln, slp) :: lex(rem)
          |    }
          |  }
          |
          |  /**
          |   * performs the transition for a given input on all DFAs and filter's out trapped ones
          |   * @param dfas the list of token, dfa pairs to transition with
          |   * @param input the transition input
          |   * @return list of non-trapped dfas after transitioning
          |   */
          |  private def transition(dfas: List[(${langName}TokenType, DFA)], input: Char): List[(${langName}TokenType, DFA)] = {
          |    dfas.foreach((_, dfa) => dfa.process(input))
          |    dfas.filter((_, dfa) => !dfa.isTrapped)
          |  }
          |}
          |
          |""".stripMargin
      )
      
      // always close your printwriters
      dfaWriter.close()
      tokenTypeWriter.close()
      lexerWriter.close()
      pw1.close()
      pw2.close()
    }
  }
