package zio.blocks.schema.json

import zio.blocks.schema.SchemaBaseSpec
import zio.blocks.schema.JavaTimeGen._
import zio.blocks.schema._
import zio.test._
import zio.test.Assertion.{containsString, isLeft}
import zio.test.TestAspect.exceptNative
import java.time._

object JsonInterpolatorSpec extends SchemaBaseSpec {
  def spec: Spec[TestEnvironment, Any] = suite("JsonInterpolatorSpec")(
    test("parses Json literal") {
      assertTrue(
        json""" "hello"""" == Json.str("hello"),
        json""""Привіт" """ == Json.str("Привіт"),
        json""" "★🎸🎧⋆｡°⋆" """ == Json.str("★🎸🎧⋆｡°⋆"),
        json"""42""" == Json.number(42),
        json"""true""" == Json.bool(true),
        json"""[1,0,-1]""" == Json.arr(Json.number(1), Json.number(0), Json.number(-1)),
        json"""{"name": "Alice", "age": 20}""" == Json.obj("name" -> Json.str("Alice"), "age" -> Json.number(20)),
        json"""null""" == Json.Null
      )
    },
    test("supports interpolated String keys and values") {
      check(
        Gen.string(Gen.char.filter(x => x <= 0xd800 || x >= 0xdfff)) // excluding surrogate chars
      )(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x),
          json"""{$x: "v"}""".get(x).string == Right("v")
        )
      ) && {
        val x = "★🎸🎧⋆｡°⋆"
        assertTrue(
          json"""{"★🎸🎧⋆｡°⋆": $x}""".get("★🎸🎧⋆｡°⋆").string == Right(x),
          json"""{$x: "★🎸🎧⋆｡°⋆"}""".get(x).string == Right("★🎸🎧⋆｡°⋆")
        )
      } && {
        val x = "★" * 100
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x),
          json"""{$x: "v"}""".get(x).string == Right("v")
        )
      }
    },
    test("supports interpolated Boolean keys and values") {
      check(Gen.boolean)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").boolean == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Byte keys and values") {
      check(Gen.byte)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").int.map(_.toByte) == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Short keys and values") {
      check(Gen.short)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").int.map(_.toShort) == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Int keys and values") {
      check(Gen.int)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").int == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Long keys and values") {
      check(Gen.long)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").long == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Float keys and values") {
      check(Gen.float)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").float == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Double keys and values") {
      check(Gen.double)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").double == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Char keys and values") {
      check(
        Gen.char.filter(x => x <= 0xd800 || x >= 0xdfff) // excluding surrogate chars
      )(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated BigDecimal keys and values") {
      check(Gen.bigDecimal(BigDecimal("-" + "9" * 100), BigDecimal("9" * 100)))(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").number == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated BigInt keys and values") {
      check(Gen.bigInt(BigInt("-" + "9" * 100), BigInt("9" * 100)))(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").number.map(_.toBigInt) == Right(x),
          json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated DayOfWeek keys and values") {
      check(genDayOfWeek)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Duration keys and values") {
      check(genDuration)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Instant keys and values") {
      check(genInstant)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated LocalDate keys and values") {
      check(genLocalDate)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated LocalDateTime keys and values") {
      check(genLocalDateTime)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated LocalTime keys and values") {
      check(genLocalTime)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Month keys and values") {
      check(genMonth)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated MonthDay keys and values") {
      check(genMonthDay)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated OffsetDateTime keys and values") {
      check(genOffsetDateTime)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated OffsetTime keys and values") {
      check(genOffsetTime)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Period keys and values") {
      check(genPeriod)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Year values") {
      check(genYear)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string.map(_.toInt) == Right(x.getValue)
        )
      )
    },
    test("supports interpolated YearMonth values") {
      check(genYearMonth)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string.map(YearMonth.parse) == Right(x)
        )
      )
    },
    test("supports interpolated ZoneOffset keys and values") {
      check(genZoneOffset)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated ZoneId keys and values") {
      check(genZoneId)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated ZonedDateTime keys and values") {
      check(genZonedDateTime)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Currency keys and values") {
      check(Gen.currency)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated UUID keys and values") {
      check(Gen.uuid)(x =>
        assertTrue(
          json"""{"x": $x}""".get("x").string == Right(x.toString),
          json"""{$x: "v"}""".get(x.toString).string == Right("v")
        )
      )
    },
    test("supports interpolated Option values") {
      val some: Option[String] = Some("Alice")
      val none: Option[String] = None
      assertTrue(
        json"""{"x": $some}""".get("x").one == Right(Json.str(some.get)),
        json"""{"x": $none}""".get("x").one == Right(Json.Null)
      )
    },
    test("supports interpolated Null values") {
      val x: String = null
      assertTrue(json"""{"x": $x}""".get("x").one == Right(Json.Null))
    },
    test("supports interpolated Unit values") {
      val x: Unit = ()
      assertTrue(json"""{"x": $x}""".get("x").one == Right(Json.obj()))
    },
    test("supports interpolated Json values") {
      val x: Json = Json.obj("y" -> Json.number(1))
      assertTrue(json"""{"x": $x}""".get("x").get("y").int == Right(1))
    },

    test("supports interpolated keys and values of other types with overridden toString") {
      case class Person(name: String, age: Int) {
        locally { val _ = (name, age) }
        override def toString: String = s"""{"name":"$name","age":$age}"""
      }

      object Person {
        implicit val schema: Schema[Person] = Schema.derived

        val jsonCodec: JsonBinaryCodec[Person] = schema.derive(JsonBinaryCodecDeriver)
      }

      val x = Person("Alice", 20)
      assertTrue(
        json"""{"x": $x}""".get("x").one == Right(Json.obj("name" -> Json.str("Alice"), "age" -> Json.number(20))),
        json"""{${x.toString}: "v"}""".get(x.toString).string == Right("v")
      )
    },
    test("doesn't compile for invalid json") {
      typeCheck {
        """json"1e""""
      }.map(assert(_)(isLeft(containsString("Invalid JSON literal: unexpected end of input at: .")))) &&
      typeCheck {
        """json"[1,02]""""
      }.map(assert(_)(isLeft(containsString("Invalid JSON literal: illegal number with leading zero at: .at(1)"))))
    } @@ exceptNative,

    // ═══════════════════════════════════════════════════════════════════════════
    // Enhanced Type-Safe Interpolation Test Suites
    // ═══════════════════════════════════════════════════════════════════════════

    suite("key position interpolation - all PrimitiveTypes")(
      test("supports all stringable types as keys") {
        val s            = "key"
        val b            = true
        val byte: Byte   = 1
        val short: Short = 2
        val int          = 3
        val long         = 4L
        val uuid         = java.util.UUID.randomUUID()
        val instant      = Instant.now()
        val localDate    = LocalDate.of(2024, 1, 15)
        val currency     = java.util.Currency.getInstance("USD")

        assertTrue(
          json"""{$s: 1}""".get(s).int == Right(1),
          json"""{$b: 1}""".get("true").int == Right(1),
          json"""{$byte: 1}""".get("1").int == Right(1),
          json"""{$short: 1}""".get("2").int == Right(1),
          json"""{$int: 1}""".get("3").int == Right(1),
          json"""{$long: 1}""".get("4").int == Right(1),
          json"""{$uuid: 1}""".get(uuid.toString).int == Right(1),
          json"""{$instant: 1}""".get(instant.toString).int == Right(1),
          json"""{$localDate: 1}""".get(localDate.toString).int == Right(1),
          json"""{$currency: 1}""".get("USD").int == Right(1)
        )
      },

      test("property-based: stringable types work as keys") {
        check(Gen.uuid) { uuid =>
          assertTrue(json"""{$uuid: "v"}""".get(uuid.toString).string == Right("v"))
        } &&
        check(Gen.int) { n =>
          assertTrue(json"""{$n: "v"}""".get(n.toString).string == Right("v"))
        } &&
        check(genInstant) { instant =>
          assertTrue(json"""{$instant: "v"}""".get(instant.toString).string == Right("v"))
        } &&
        check(genLocalDate) { date =>
          assertTrue(json"""{$date: "v"}""".get(date.toString).string == Right("v"))
        }
      }
    ),

    suite("string literal interpolation")(
      test("supports String interpolation in strings") {
        val name = "Alice"
        assertTrue(
          json"""{"greeting": "Hello, $name!"}""".get("greeting").string == Right("Hello, Alice!")
        )
      },

      test("supports numeric types in strings") {
        val x   = 42
        val y   = 3.14
        val big = BigInt("12345678901234567890")

        assertTrue(
          json"""{"msg": "x is $x"}""".get("msg").string == Right("x is 42"),
          json"""{"msg": "y is $y"}""".get("msg").string == Right("y is 3.14"),
          json"""{"msg": "big is $big"}""".get("msg").string == Right("big is 12345678901234567890")
        )
      },

      test("supports UUID in strings") {
        val id = java.util.UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
        assertTrue(
          json"""{"ref": "user-$id"}""".get("ref").string == Right("user-550e8400-e29b-41d4-a716-446655440000")
        )
      },

      test("supports temporal types in strings") {
        val date    = LocalDate.of(2024, 1, 15)
        val time    = LocalTime.of(10, 30, 0)
        val instant = Instant.parse("2024-01-15T10:30:00Z")

        assertTrue(
          json"""{"file": "report-$date.pdf"}""".get("file").string == Right("report-2024-01-15.pdf"),
          json"""{"log": "Event at $time"}""".get("log").string == Right("Event at 10:30"),
          json"""{"ts": "Created: $instant"}""".get("ts").string == Right("Created: 2024-01-15T10:30:00Z")
        )
      },

      test("supports Currency in strings") {
        val currency = java.util.Currency.getInstance("USD")
        assertTrue(
          json"""{"label": "Price in $currency"}""".get("label").string == Right("Price in USD")
        )
      },

      test("supports multiple interpolations in one string") {
        val date    = LocalDate.of(2024, 1, 15)
        val version = 3
        val env     = "prod"

        assertTrue(
          json"""{"path": "/data/$env/$date/v$version/output.json"}""".get("path").string ==
            Right("/data/prod/2024-01-15/v3/output.json")
        )
      },

      test("handles empty interpolation results") {
        val empty = ""
        assertTrue(
          json"""{"msg": "[$empty]"}""".get("msg").string == Right("[]")
        )
      },

      test("handles special characters in interpolated strings") {
        val path  = "foo/bar"
        val query = "a=1&b=2"

        assertTrue(
          json"""{"url": "http://example.com/$path?$query"}""".get("url").string ==
            Right("http://example.com/foo/bar?a=1&b=2")
        )
      }
    ),

    suite("mixed interpolation contexts")(
      test("combines key and string interpolation") {
        val key       = java.util.UUID.randomUUID()
        val timestamp = Instant.now()

        val result = json"""{
          $key: {
            "value": 42,
            "note": "Recorded at $timestamp"
          }
        }"""

        assertTrue(
          result.get(key.toString).get("value").int == Right(42),
          result.get(key.toString).get("note").string == Right(s"Recorded at $timestamp")
        )
      },

      test("multiple keys with different stringable types") {
        val intKey  = 1
        val uuidKey = java.util.UUID.randomUUID()
        val dateKey = LocalDate.of(2024, 1, 15)

        val result = json"""{
          $intKey: "one",
          $uuidKey: "uuid",
          $dateKey: "date"
        }"""

        assertTrue(
          result.get("1").string == Right("one"),
          result.get(uuidKey.toString).string == Right("uuid"),
          result.get("2024-01-15").string == Right("date")
        )
      },

      test("string interpolation with various types") {
        val name   = "Alice"
        val age    = 30
        val score  = 95.5
        val active = true

        val result = json"""{
          "message": "User $name, age $age, score $score, active: $active"
        }"""

        assertTrue(
          result.get("message").string == Right("User Alice, age 30, score 95.5, active: true")
        )
      }
    )
  )
}
