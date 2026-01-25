package zio.blocks.schema.json

import java.io.OutputStream
import java.time._
import java.util.{Currency, UUID}
import scala.annotation.tailrec

/**
 * Shared runtime utilities for JSON string interpolation. Used by both Scala 2
 * and Scala 3 macro implementations.
 */
object JsonInterpolatorRuntime {
  def jsonWithInterpolation(sc: StringContext, args: Seq[Any]): Json = {
    val parts      = sc.parts.toIndexedSeq
    val out        = new ByteArrayOutputStream(parts.map(_.length).sum << 1)
    var quoteCount = 0

    // Write first part
    val firstPart = parts(0)
    out.write(firstPart)
    quoteCount += countUnescapedQuotes(firstPart)

    // Process each argument with its surrounding parts
    var idx = 0
    while (idx < args.length) {
      val arg   = args(idx)
      val after = parts(idx + 1)

      // If quoteCount is odd, we are inside a string literal
      val insideString = quoteCount % 2 == 1

      if (insideString) {
        // Inside a string literal - write as plain string
        writeStringValue(out, arg)
      } else {
        // Outside string literal - check if it's a key or value
        val beforeTrimmed = parts(idx).trim
        val afterTrimmed  = after.trim
        val isKey         = (beforeTrimmed.endsWith("{") || beforeTrimmed.endsWith(",")) && afterTrimmed.startsWith(":")

        if (isKey) writeKey(out, arg)
        else writeValue(out, arg)
      }

      // Write the next part
      out.write(after)
      quoteCount += countUnescapedQuotes(after)
      idx += 1
    }

    Json.jsonCodec.decode(out.toByteArray) match {
      case Right(json) => json
      case Left(error) => throw error
    }
  }

  /**
   * Counts unescaped double quotes in a string.
   */
  private[this] def countUnescapedQuotes(s: String): Int = {
    var count = 0
    var i     = 0
    while (i < s.length) {
      if (s.charAt(i) == '"') {
        // Count preceding backslashes
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

  /**
   * Writes a value as a plain string (for interpolation inside JSON string
   * literals). Uses Stringable typeclass to convert the value to string.
   */
  private[this] def writeStringValue(out: ByteArrayOutputStream, value: Any): Unit = value match {
    case s: String           => out.write(s.getBytes("UTF-8"))
    case b: Boolean          => out.write(b.toString.getBytes("UTF-8"))
    case b: Byte             => out.write(b.toString.getBytes("UTF-8"))
    case sh: Short           => out.write(sh.toString.getBytes("UTF-8"))
    case i: Int              => out.write(i.toString.getBytes("UTF-8"))
    case l: Long             => out.write(l.toString.getBytes("UTF-8"))
    case f: Float            => out.write(f.toString.getBytes("UTF-8"))
    case d: Double           => out.write(d.toString.getBytes("UTF-8"))
    case c: Char             => out.write(c.toString.getBytes("UTF-8"))
    case bd: BigDecimal      => out.write(bd.toString.getBytes("UTF-8"))
    case bi: BigInt          => out.write(bi.toString.getBytes("UTF-8"))
    case dow: DayOfWeek      => out.write(dow.toString.getBytes("UTF-8"))
    case d: Duration         => out.write(d.toString.getBytes("UTF-8"))
    case i: Instant          => out.write(i.toString.getBytes("UTF-8"))
    case ld: LocalDate       => out.write(ld.toString.getBytes("UTF-8"))
    case ldt: LocalDateTime  => out.write(ldt.toString.getBytes("UTF-8"))
    case lt: LocalTime       => out.write(lt.toString.getBytes("UTF-8"))
    case m: Month            => out.write(m.toString.getBytes("UTF-8"))
    case md: MonthDay        => out.write(md.toString.getBytes("UTF-8"))
    case odt: OffsetDateTime => out.write(odt.toString.getBytes("UTF-8"))
    case ot: OffsetTime      => out.write(ot.toString.getBytes("UTF-8"))
    case p: Period           => out.write(p.toString.getBytes("UTF-8"))
    case y: Year             => out.write(y.toString.getBytes("UTF-8"))
    case ym: YearMonth       => out.write(ym.toString.getBytes("UTF-8"))
    case zo: ZoneOffset      => out.write(zo.toString.getBytes("UTF-8"))
    case zi: ZoneId          => out.write(zi.toString.getBytes("UTF-8"))
    case zdt: ZonedDateTime  => out.write(zdt.toString.getBytes("UTF-8"))
    case c: Currency         => out.write(c.getCurrencyCode.getBytes("UTF-8"))
    case uuid: UUID          => out.write(uuid.toString.getBytes("UTF-8"))
    case x                   => out.write(x.toString.getBytes("UTF-8"))
  }

  private[this] def writeValue(out: ByteArrayOutputStream, value: Any): Unit = value match {
    case s: String             => JsonBinaryCodec.stringCodec.encode(s, out)
    case b: Boolean            => JsonBinaryCodec.booleanCodec.encode(b, out)
    case b: Byte               => JsonBinaryCodec.byteCodec.encode(b, out)
    case sh: Short             => JsonBinaryCodec.shortCodec.encode(sh, out)
    case i: Int                => JsonBinaryCodec.intCodec.encode(i, out)
    case l: Long               => JsonBinaryCodec.longCodec.encode(l, out)
    case f: Float              => JsonBinaryCodec.floatCodec.encode(f, out)
    case d: Double             => JsonBinaryCodec.doubleCodec.encode(d, out)
    case c: Char               => JsonBinaryCodec.charCodec.encode(c, out)
    case bd: BigDecimal        => JsonBinaryCodec.bigDecimalCodec.encode(bd, out)
    case bi: BigInt            => JsonBinaryCodec.bigIntCodec.encode(bi, out)
    case dow: DayOfWeek        => JsonBinaryCodec.dayOfWeekCodec.encode(dow, out)
    case d: Duration           => JsonBinaryCodec.durationCodec.encode(d, out)
    case i: Instant            => JsonBinaryCodec.instantCodec.encode(i, out)
    case ld: LocalDate         => JsonBinaryCodec.localDateCodec.encode(ld, out)
    case ldt: LocalDateTime    => JsonBinaryCodec.localDateTimeCodec.encode(ldt, out)
    case lt: LocalTime         => JsonBinaryCodec.localTimeCodec.encode(lt, out)
    case m: Month              => JsonBinaryCodec.monthCodec.encode(m, out)
    case md: MonthDay          => JsonBinaryCodec.monthDayCodec.encode(md, out)
    case odt: OffsetDateTime   => JsonBinaryCodec.offsetDateTimeCodec.encode(odt, out)
    case ot: OffsetTime        => JsonBinaryCodec.offsetTimeCodec.encode(ot, out)
    case p: Period             => JsonBinaryCodec.periodCodec.encode(p, out)
    case y: Year               => JsonBinaryCodec.yearCodec.encode(y, out)
    case ym: YearMonth         => JsonBinaryCodec.yearMonthCodec.encode(ym, out)
    case zo: ZoneOffset        => JsonBinaryCodec.zoneOffsetCodec.encode(zo, out)
    case zi: ZoneId            => JsonBinaryCodec.zoneIdCodec.encode(zi, out)
    case zdt: ZonedDateTime    => JsonBinaryCodec.zonedDateTimeCodec.encode(zdt, out)
    case c: java.util.Currency => JsonBinaryCodec.currencyCodec.encode(c, out)
    case uuid: java.util.UUID  => JsonBinaryCodec.uuidCodec.encode(uuid, out)
    case opt: Option[_]        =>
      opt match {
        case Some(value) => writeValue(out, value)
        case _           =>
          out.write('n')
          out.write('u')
          out.write('l')
          out.write('l')
      }
    case null =>
      out.write('n')
      out.write('u')
      out.write('l')
      out.write('l')
    case _: Unit =>
      out.write('{')
      out.write('}')
    case j: Json                         => Json.jsonCodec.encode(j, out)
    case map: scala.collection.Map[_, _] =>
      out.write('{')
      map.foreach {
        var comma = false
        kv =>
          if (comma) out.write(',')
          else comma = true
          writeKey(out, kv._1)
          out.write(':')
          writeValue(out, kv._2)
      }
      out.write('}')
    case seq: Iterable[_] =>
      out.write('[')
      seq.foreach {
        var comma = false
        x =>
          if (comma) out.write(',')
          else comma = true
          writeValue(out, x)
      }
      out.write(']')
    case arr: Array[_] =>
      out.write('[')
      val len = arr.length
      var idx = 0
      while (idx < len) {
        if (idx > 0) out.write(',')
        writeValue(out, arr(idx))
        idx += 1
      }
      out.write(']')
    case x => out.write(x.toString)
  }

  private[this] def writeKey(out: ByteArrayOutputStream, key: Any): Unit =
    key match {
      case s: String  => JsonBinaryCodec.stringCodec.encode(s, out)
      case b: Boolean =>
        out.write('"')
        JsonBinaryCodec.booleanCodec.encode(b, out)
        out.write('"')
      case b: Byte =>
        out.write('"')
        JsonBinaryCodec.byteCodec.encode(b, out)
        out.write('"')
      case sh: Short =>
        out.write('"')
        JsonBinaryCodec.shortCodec.encode(sh, out)
        out.write('"')
      case i: Int =>
        out.write('"')
        JsonBinaryCodec.intCodec.encode(i, out)
        out.write('"')
      case l: Long =>
        out.write('"')
        JsonBinaryCodec.longCodec.encode(l, out)
        out.write('"')
      case f: Float =>
        out.write('"')
        JsonBinaryCodec.floatCodec.encode(f, out)
        out.write('"')
      case d: Double =>
        out.write('"')
        JsonBinaryCodec.doubleCodec.encode(d, out)
        out.write('"')
      case bd: BigDecimal =>
        out.write('"')
        JsonBinaryCodec.bigDecimalCodec.encode(bd, out)
        out.write('"')
      case bi: BigInt =>
        out.write('"')
        JsonBinaryCodec.bigIntCodec.encode(bi, out)
        out.write('"')
      case d: Duration         => JsonBinaryCodec.durationCodec.encode(d, out)
      case dow: DayOfWeek      => JsonBinaryCodec.dayOfWeekCodec.encode(dow, out)
      case i: Instant          => JsonBinaryCodec.instantCodec.encode(i, out)
      case ld: LocalDate       => JsonBinaryCodec.localDateCodec.encode(ld, out)
      case ldt: LocalDateTime  => JsonBinaryCodec.localDateTimeCodec.encode(ldt, out)
      case lt: LocalTime       => JsonBinaryCodec.localTimeCodec.encode(lt, out)
      case m: Month            => JsonBinaryCodec.monthCodec.encode(m, out)
      case md: MonthDay        => JsonBinaryCodec.monthDayCodec.encode(md, out)
      case odt: OffsetDateTime => JsonBinaryCodec.offsetDateTimeCodec.encode(odt, out)
      case ot: OffsetTime      => JsonBinaryCodec.offsetTimeCodec.encode(ot, out)
      case p: Period           => JsonBinaryCodec.periodCodec.encode(p, out)
      case y: Year             => JsonBinaryCodec.yearCodec.encode(y, out)
      case ym: YearMonth       => JsonBinaryCodec.yearMonthCodec.encode(ym, out)
      case zo: ZoneOffset      => JsonBinaryCodec.zoneOffsetCodec.encode(zo, out)
      case zi: ZoneId          => JsonBinaryCodec.zoneIdCodec.encode(zi, out)
      case zdt: ZonedDateTime  => JsonBinaryCodec.zonedDateTimeCodec.encode(zdt, out)
      case c: Currency         => JsonBinaryCodec.currencyCodec.encode(c, out)
      case uuid: UUID          => JsonBinaryCodec.uuidCodec.encode(uuid, out)
      case x                   => JsonBinaryCodec.stringCodec.encode(x.toString, out)
    }
}

private class ByteArrayOutputStream(initCapacity: Int) extends OutputStream {
  private[this] var buf   = new Array[Byte](initCapacity)
  private[this] var count = 0

  override def write(bytes: Array[Byte], off: Int, len: Int): Unit = {
    val limit = count + len
    if (limit > buf.length) buf = java.util.Arrays.copyOf(buf, math.max(buf.length << 1, limit))
    System.arraycopy(bytes, off, buf, count, len)
    count = limit
  }

  override def write(b: Int): Unit = {
    val pos = count
    if (pos >= buf.length) buf = java.util.Arrays.copyOf(buf, buf.length << 1)
    buf(pos) = b.toByte
    count = pos + 1
  }

  def write(s: String): Unit = count = write(s, 0, s.length, count, buf.length - 4)

  @tailrec
  private[this] def write(s: String, from: Int, to: Int, pos: Int, posLim: Int): Int =
    if (from >= to) pos
    else if (pos >= posLim) {
      buf = java.util.Arrays.copyOf(buf, Math.max(buf.length << 1, count + (to - from) * 3))
      write(s, from, to, pos, buf.length - 4)
    } else {
      val ch1 = s.charAt(from).toInt
      if (ch1 < 0x80) {
        buf(pos) = ch1.toByte
        write(s, from + 1, to, pos + 1, posLim)
      } else if (ch1 < 0x800) { // 00000bbbbbaaaaaa (UTF-16 char) -> 110bbbbb 10aaaaaa (UTF-8 bytes)
        buf(pos) = (ch1 >> 6 | 0xc0).toByte
        buf(pos + 1) = (ch1 & 0x3f | 0x80).toByte
        write(s, from + 1, to, pos + 2, posLim)
      } else if ((ch1 & 0xf800) != 0xd800) { // ccccbbbbbbaaaaaa (UTF-16 char) -> 1110cccc 10bbbbbb 10aaaaaa (UTF-8 bytes)
        buf(pos) = (ch1 >> 12 | 0xe0).toByte
        buf(pos + 1) = (ch1 >> 6 & 0x3f | 0x80).toByte
        buf(pos + 2) = (ch1 & 0x3f | 0x80).toByte
        write(s, from + 1, to, pos + 3, posLim)
      } else { // 110110uuuuccccbb 110111bbbbaaaaaa (UTF-16 chars) -> 11110ddd 10ddcccc 10bbbbbb 10aaaaaa (UTF-8 bytes), where ddddd = uuuu + 1
        var ch2 = 0
        if (
          ch1 >= 0xdc00 || from + 1 >= to || {
            ch2 = s.charAt(from + 1).toInt
            (ch2 & 0xfc00) != 0xdc00
          }
        ) throw new JsonBinaryCodecError(Nil, "Illegal surrogate pair")
        val cp = (ch1 << 10) + (ch2 - 56613888) // -56613888 == 0x10000 - (0xD800 << 10) - 0xDC00
        buf(pos) = (cp >> 18 | 0xf0).toByte
        buf(pos + 1) = (cp >> 12 & 0x3f | 0x80).toByte
        buf(pos + 2) = (cp >> 6 & 0x3f | 0x80).toByte
        buf(pos + 3) = (cp & 0x3f | 0x80).toByte
        write(s, from + 2, to, pos + 4, posLim)
      }
    }

  def toByteArray: Array[Byte] = java.util.Arrays.copyOf(buf, count)
}
