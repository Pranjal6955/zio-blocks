package zio.blocks.schema

import zio.blocks.schema.json._
import scala.language.experimental.macros
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

package object json {
  implicit class JsonStringContext(val sc: StringContext) extends AnyVal {
    def json(args: Any*): Json = macro JsonInterpolatorMacros.jsonImpl
  }
}

private object JsonInterpolatorMacros {
  def jsonImpl(c: blackbox.Context)(args: c.Expr[Any]*): c.Expr[Json] = {
    import c.universe._

    val parts = c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        rawParts.map {
          case Literal(Constant(part: String)) => part
          case _                               => c.abort(c.enclosingPosition, "Expected string literal parts")
        }
      case _ => c.abort(c.enclosingPosition, "Expected StringContext")
    }

    // Validate JSON literal syntax at compile-time
    try {
      JsonInterpolatorRuntime.jsonWithInterpolation(new StringContext(parts: _*), (2 to parts.size).map(_ => ""))
    } catch {
      case error if NonFatal(error) => c.abort(c.enclosingPosition, s"Invalid JSON literal: ${error.getMessage}")
    }

    // Type-check each interpolation based on context
    args.zipWithIndex.foreach { case (arg, idx) =>
      val argType = arg.actualType
      val before  = if (idx < parts.length) parts(idx) else ""
      val after   = if (idx + 1 < parts.length) parts(idx + 1) else ""

      val context = detectInterpolationContext(before, after)

      context match {
        case InterpolationContext.StringLiteral =>
          // Inside a string literal - require Stringable
          val stringableType     = appliedType(typeOf[Stringable[_]].typeConstructor, List(argType))
          val stringableImplicit = c.inferImplicitValue(stringableType, silent = true)

          if (stringableImplicit == EmptyTree) {
            c.abort(
              arg.tree.pos,
              s"Cannot interpolate type ${argType} in string literal. " +
                s"Only stringable types (primitives, BigInt, BigDecimal, java.time types, UUID, Currency) are allowed. " +
                s"Provide an implicit Stringable[${argType}] instance."
            )
          }

        case InterpolationContext.Key =>
          // Key position - require Stringable
          val stringableType     = appliedType(typeOf[Stringable[_]].typeConstructor, List(argType))
          val stringableImplicit = c.inferImplicitValue(stringableType, silent = true)

          if (stringableImplicit == EmptyTree) {
            c.abort(
              arg.tree.pos,
              s"Cannot use type ${argType} in key position. " +
                s"Only stringable types (primitives, BigInt, BigDecimal, java.time types, UUID, Currency) are allowed. " +
                s"Provide an implicit Stringable[${argType}] instance."
            )
          }

        case InterpolationContext.Value =>
          // Value position - require JsonEncoder
          val encoderType     = appliedType(typeOf[JsonEncoder[_]].typeConstructor, List(argType))
          val encoderImplicit = c.inferImplicitValue(encoderType, silent = true)

          if (encoderImplicit == EmptyTree) {
            c.abort(
              arg.tree.pos,
              s"No JsonEncoder available for type ${argType}. " +
                s"Provide an implicit JsonEncoder[${argType}] or Schema[${argType}] instance."
            )
          }
      }
    }

    val scExpr   = c.Expr[StringContext](c.prefix.tree.asInstanceOf[Apply].args.head)
    val argsExpr = c.Expr[Seq[Any]](q"Seq(..$args)")
    reify(JsonInterpolatorRuntime.jsonWithInterpolation(scExpr.splice, argsExpr.splice))
  }

  private sealed trait InterpolationContext
  private object InterpolationContext {
    case object StringLiteral extends InterpolationContext
    case object Key           extends InterpolationContext
    case object Value         extends InterpolationContext
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
