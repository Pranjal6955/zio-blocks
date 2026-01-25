package zio.blocks.schema

import zio.blocks.schema.json._
import scala.quoted._
import scala.util.control.NonFatal

package object json {
  extension (inline sc: StringContext) {
    inline def json(inline args: Any*): Json = ${ jsonInterpolatorImpl('sc, 'args) }
  }

  private def jsonInterpolatorImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[Json] = {
    import quotes.reflect._

    val parts = sc match {
      case '{ StringContext(${ Varargs(rawParts) }: _*) } =>
        rawParts.map { case '{ $rawPart: String } => rawPart.valueOrAbort }
      case _ => report.errorAndAbort("Expected a StringContext with string literal parts")
    }

    // Validate JSON literal syntax at compile-time
    try {
      JsonInterpolatorRuntime.jsonWithInterpolation(new StringContext(parts: _*), (2 to parts.size).map(_ => ""))
    } catch {
      case error if NonFatal(error) => report.errorAndAbort(s"Invalid JSON literal: ${error.getMessage}")
    }

    // Type-check each interpolation based on context
    args match {
      case Varargs(argExprs) =>
        argExprs.zipWithIndex.foreach { case (argExpr, idx) =>
          val argType = argExpr.asTerm.tpe.widen
          val before  = if (idx < parts.length) parts(idx) else ""
          val after   = if (idx + 1 < parts.length) parts(idx + 1) else ""

          val context = detectInterpolationContext(before, after)

          context match {
            case InterpolationContext.StringLiteral =>
              // Inside a string literal - require Stringable
              argType.asType match {
                case '[t] =>
                  Expr.summon[Stringable[t]] match {
                    case Some(_) => // OK
                    case None    =>
                      report.errorAndAbort(
                        s"Cannot interpolate type ${argType.show} in string literal. " +
                          s"Only stringable types (primitives, BigInt, BigDecimal, java.time types, UUID, Currency) are allowed. " +
                          s"Provide an implicit Stringable[${argType.show}] instance.",
                        argExpr.asTerm.pos
                      )
                  }
              }

            case InterpolationContext.Key =>
              // Key position - require Stringable
              argType.asType match {
                case '[t] =>
                  Expr.summon[Stringable[t]] match {
                    case Some(_) => // OK
                    case None    =>
                      report.errorAndAbort(
                        s"Cannot use type ${argType.show} in key position. " +
                          s"Only stringable types (primitives, BigInt, BigDecimal, java.time types, UUID, Currency) are allowed. " +
                          s"Provide an implicit Stringable[${argType.show}] instance.",
                        argExpr.asTerm.pos
                      )
                  }
              }

            case InterpolationContext.Value =>
              // Value position - require JsonEncoder
              argType.asType match {
                case '[t] =>
                  Expr.summon[JsonEncoder[t]] match {
                    case Some(_) => // OK
                    case None    =>
                      report.errorAndAbort(
                        s"No JsonEncoder available for type ${argType.show}. " +
                          s"Provide an implicit JsonEncoder[${argType.show}] or Schema[${argType.show}] instance.",
                        argExpr.asTerm.pos
                      )
                  }
              }
          }
        }
      case _ => ()
    }

    '{ JsonInterpolatorRuntime.jsonWithInterpolation($sc, $args) }
  }

  private enum InterpolationContext {
    case StringLiteral
    case Key
    case Value
  }

  /**
   * Detects the interpolation context by analyzing the surrounding JSON text.
   */
  private def detectInterpolationContext(before: String, after: String): InterpolationContext = {
    // Remove leading/trailing whitespace to simplify detection
    val beforeTrimmed = before.trim
    val afterTrimmed  = after.trim

    // Check if we're inside a string literal
    // Count unescaped quotes in the 'before' part
    val quoteCount = countUnescapedQuotes(before)
    if (quoteCount % 2 == 1) {
      // Odd number of quotes means we're inside a string literal
      return InterpolationContext.StringLiteral
    }

    // Check if this is a key position (before ':')
    // Key position: after '{' or ',' and before ':'
    val isAfterObjectStart = beforeTrimmed.endsWith("{") || beforeTrimmed.endsWith(",")
    val isBeforeColon      = afterTrimmed.startsWith(":")

    if (isAfterObjectStart && isBeforeColon) {
      return InterpolationContext.Key
    }

    // Otherwise, it's a value position
    InterpolationContext.Value
  }

  /**
   * Counts unescaped double quotes in a string.
   */
  private def countUnescapedQuotes(s: String): Int = {
    var count = 0
    var i     = 0
    while (i < s.length) {
      if (s.charAt(i) == '"') {
        // Check if it's escaped by counting preceding backslashes
        var backslashCount = 0
        var j              = i - 1
        while (j >= 0 && s.charAt(j) == '\\') {
          backslashCount += 1
          j -= 1
        }
        // If even number of backslashes (including 0), the quote is not escaped
        if (backslashCount % 2 == 0) {
          count += 1
        }
      }
      i += 1
    }
    count
  }
}
