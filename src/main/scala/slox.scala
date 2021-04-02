package github.parzival3.lox

import scala.io.Source
import java.io.InputStreamReader
import java.util
import Console.{BLUE, GREEN, RED, RESET}
import scala.annotation.tailrec

class slox:
  // TODO: please change this

  @tailrec
  final def run(program: String, hadError: Boolean): Exit =
    val scanner = Scanner(program)
    val scannerResult = scanner.scanTokens(program.toList, -1, -1, 1, Nil)
    scannerResult match
      case Right(tokens) =>
        tokens.map(println)
        if !hadError then Exit.SUCCESS else Exit.EX_USAGE
      case Left(start, current, line) =>
        error(line, "Unexpected character.")
        // If there is an error let's catch as many error as possible
        run(program.substring(current), true)

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def report(line: Int, where: String, message: String): Unit =
    System.err.println(s"[line $line] Error $where: $message")

  def runFile(program: String): Exit =
    run(Source.fromFile(program, "UTF-8").toString(), false)
  
  @tailrec
  final def runPrompt(): Exit =
    print(s"${GREEN}> ")
    val line = io.StdIn.readLine()
    if !line.isBlank then
      run(line, false)
      runPrompt()
    else
      Exit.SUCCESS


@main def parse(args: String*): Exit =
  val usage: String = "Usage: slox [script]"
  val my_slox = slox()

  if args.length > 1 then
    println(usage)
    Exit.EX_USAGE
  else if args.length == 1 then
    my_slox.runFile(args(0))
  else
    my_slox.runPrompt()
