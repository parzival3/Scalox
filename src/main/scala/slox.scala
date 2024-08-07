package github.parzival3.lox

import scala.io.Source
import java.io.InputStreamReader
import java.util
import Console.{BLUE, GREEN, RED, RESET}
import scala.annotation.tailrec

class slox:

  @tailrec
  final def run(program: String, hadError: Boolean): Exit =
    val scanner = Scanner(program)
    val scannerResult = scanner.scanTokens(ScannerState(program.toList, 0, 1, Nil))
    scannerResult match
      case Right(tokens) =>
        println(prettyPrint(Parser(tokens).parse.expr.get))
        if !hadError then Exit.SUCCESS else Exit.EX_USAGE
      case Left(start, line) =>
        error(line, "Unexpected character.")
        // If there is an error let's catch as many error as possible
        run(program.substring(start + 1), true)

  def error(line: Int, message: String): Unit =
    report(line, "", message)

  def report(line: Int, where: String, message: String): Unit =
    System.err.println(s"[line $line] Error $where: $message")

  def runFile(program: String): Exit =
    val stringProgram: String = Source.fromFile(program, "UTF-8").getLines.mkString
    run(stringProgram, false)

  @tailrec
  final def runPrompt(): Exit =
    print(s"${GREEN}> ")
    val line = io.StdIn.readLine()
    if !line.isEmpty then
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
