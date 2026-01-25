package zio.blocks.schema.json

import java.time._
import java.util.{Currency, UUID}

/**
 * Typeclass for types that have a canonical string representation.
 *
 * These are the "stringable" types that can be used in:
 *   - JSON object key positions
 *   - String literal interpolations within JSON values
 *
 * All PrimitiveType types are stringable.
 */
trait Stringable[A] {
  def asString(a: A): String
}

object Stringable {

  // ═══════════════════════════════════════════════════════════════════════════
  // Scala Primitives
  // ═══════════════════════════════════════════════════════════════════════════

  implicit val unitStringable: Stringable[Unit] = new Stringable[Unit] {
    def asString(a: Unit): String = "()"
  }

  implicit val booleanStringable: Stringable[Boolean] = new Stringable[Boolean] {
    def asString(a: Boolean): String = a.toString
  }

  implicit val byteStringable: Stringable[Byte] = new Stringable[Byte] {
    def asString(a: Byte): String = a.toString
  }

  implicit val shortStringable: Stringable[Short] = new Stringable[Short] {
    def asString(a: Short): String = a.toString
  }

  implicit val intStringable: Stringable[Int] = new Stringable[Int] {
    def asString(a: Int): String = a.toString
  }

  implicit val longStringable: Stringable[Long] = new Stringable[Long] {
    def asString(a: Long): String = a.toString
  }

  implicit val floatStringable: Stringable[Float] = new Stringable[Float] {
    def asString(a: Float): String = a.toString
  }

  implicit val doubleStringable: Stringable[Double] = new Stringable[Double] {
    def asString(a: Double): String = a.toString
  }

  implicit val charStringable: Stringable[Char] = new Stringable[Char] {
    def asString(a: Char): String = a.toString
  }

  implicit val stringStringable: Stringable[String] = new Stringable[String] {
    def asString(a: String): String = a
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Arbitrary Precision Numbers
  // ═══════════════════════════════════════════════════════════════════════════

  implicit val bigIntStringable: Stringable[BigInt] = new Stringable[BigInt] {
    def asString(a: BigInt): String = a.toString
  }

  implicit val bigDecimalStringable: Stringable[BigDecimal] = new Stringable[BigDecimal] {
    def asString(a: BigDecimal): String = a.toString
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Java Time Types
  // ═══════════════════════════════════════════════════════════════════════════

  implicit val dayOfWeekStringable: Stringable[DayOfWeek] = new Stringable[DayOfWeek] {
    def asString(a: DayOfWeek): String = a.toString
  }

  implicit val durationStringable: Stringable[Duration] = new Stringable[Duration] {
    def asString(a: Duration): String = a.toString
  }

  implicit val instantStringable: Stringable[Instant] = new Stringable[Instant] {
    def asString(a: Instant): String = a.toString
  }

  implicit val localDateStringable: Stringable[LocalDate] = new Stringable[LocalDate] {
    def asString(a: LocalDate): String = a.toString
  }

  implicit val localDateTimeStringable: Stringable[LocalDateTime] = new Stringable[LocalDateTime] {
    def asString(a: LocalDateTime): String = a.toString
  }

  implicit val localTimeStringable: Stringable[LocalTime] = new Stringable[LocalTime] {
    def asString(a: LocalTime): String = a.toString
  }

  implicit val monthStringable: Stringable[Month] = new Stringable[Month] {
    def asString(a: Month): String = a.toString
  }

  implicit val monthDayStringable: Stringable[MonthDay] = new Stringable[MonthDay] {
    def asString(a: MonthDay): String = a.toString
  }

  implicit val offsetDateTimeStringable: Stringable[OffsetDateTime] = new Stringable[OffsetDateTime] {
    def asString(a: OffsetDateTime): String = a.toString
  }

  implicit val offsetTimeStringable: Stringable[OffsetTime] = new Stringable[OffsetTime] {
    def asString(a: OffsetTime): String = a.toString
  }

  implicit val periodStringable: Stringable[Period] = new Stringable[Period] {
    def asString(a: Period): String = a.toString
  }

  implicit val yearStringable: Stringable[Year] = new Stringable[Year] {
    def asString(a: Year): String = a.toString
  }

  implicit val yearMonthStringable: Stringable[YearMonth] = new Stringable[YearMonth] {
    def asString(a: YearMonth): String = a.toString
  }

  implicit val zoneIdStringable: Stringable[ZoneId] = new Stringable[ZoneId] {
    def asString(a: ZoneId): String = a.toString
  }

  implicit val zoneOffsetStringable: Stringable[ZoneOffset] = new Stringable[ZoneOffset] {
    def asString(a: ZoneOffset): String = a.toString
  }

  implicit val zonedDateTimeStringable: Stringable[ZonedDateTime] = new Stringable[ZonedDateTime] {
    def asString(a: ZonedDateTime): String = a.toString
  }

  // ═══════════════════════════════════════════════════════════════════════════
  // Java Util Types
  // ═══════════════════════════════════════════════════════════════════════════

  implicit val currencyStringable: Stringable[Currency] = new Stringable[Currency] {
    def asString(a: Currency): String = a.toString
  }

  implicit val uuidStringable: Stringable[UUID] = new Stringable[UUID] {
    def asString(a: UUID): String = a.toString
  }
}
