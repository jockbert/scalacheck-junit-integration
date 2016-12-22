package com.kastrull.scalachecktojunit

import org.scalacheck.Properties
import org.junit.Test
import org.scalacheck.Test.Parameters
import org.scalacheck.Prop._
import scala.annotation.tailrec
import org.scalacheck.Test.Result
import org.scalacheck.Test.Failed
import org.scalacheck.{ Test => CheckTest }
import org.junit.Assert._

/**
 * Use just as ordinary [[org.scalacheck.Properties]], but can be run directly by jUnit.
 *
 */
abstract class PropertiesToJUnit(name: String) extends Properties(name) {

  @Test
  def testScalaCheckProperies: Unit = {
    val params = Parameters.defaultVerbose

    this.properties.map {
      case (name, prop) =>
        CheckTest.check(params, prop) match {
          case Result(Failed(args, labels), succ, disc, freqMap, millisecs) =>
            val argText = args.zipWithIndex.map(Function.tupled(indexMissingLabel)).map(formatArg).mkString;
            val text = "\n\n! " + name + " falsified after " + succ + " passed tests.\n" + argText + "\n \n"
            fail(text)
          case _ => ()
        }
    }
  }

  private def indexMissingLabel(a: Arg[Any], i: Int): Arg[Any] = {
    if (a.label.isEmpty()) a.copy(label = "ARG_" + i)
    else a
  }

  private def formatArg(argument: Arg[Any]): String =
    argument match {
      case Arg(label, arg, 0, origArg, prettyArg, prettyOrigArg) =>
        "> " + label + ":" + pretty(arg) + "\n"
      case Arg(label, arg, shrinks, origArg, prettyArg, prettyOrigArg) =>
        "> " + label + ":" + pretty(arg) + "\n> " + label + "_ORIGINAL: " + pretty(origArg) + "\n"
    }

  private def pretty(a: Any): String =
    if (a.isInstanceOf[String]) {
      val aStr = a.toString()
      if (aStr.length() == 1 && aStr.codePointAt(0) < 0x0020)
        "\"\\u" + prePad(aStr.codePointAt(0).toHexString, 4, "0") + "\""
      else "\"" + a + "\""
    } else a.toString()

  @tailrec
  private def prePad(s: String, length: Int, padding: String): String = {
    if (s.length() >= length) s
    else prePad(padding + s, length, padding)
  }
}
