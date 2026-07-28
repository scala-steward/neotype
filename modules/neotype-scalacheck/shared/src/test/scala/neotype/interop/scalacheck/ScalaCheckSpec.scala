package neotype.interop.scalacheck

import neotype.test.definitions.*
import org.scalacheck.{Arbitrary, Gen, Prop, Test}
import scala.compiletime.testing.typeCheckErrors
import zio.test.*

object ScalaCheckSpec extends ZIOSpecDefault:

  final case class Composed(newtype: SimpleNewtype, subtype: SimpleSubtype)

  given Arbitrary[Composed] = Arbitrary {
    for
      newtype <- Arbitrary.arbitrary[SimpleNewtype]
      subtype <- Arbitrary.arbitrary[SimpleSubtype]
    yield Composed(newtype, subtype)
  }

  def spec =
    suite("ScalaCheckSpec")(
      test("derives Arbitrary for simple newtypes and subtypes") {
        val result = Test.check(
          Test.Parameters.default.withMinSuccessfulTests(100),
          Prop.forAll { (value: Composed) =>
            value.newtype == value.newtype && value.subtype == value.subtype
          }
        )

        assertTrue(result.passed)
      },
      test("does not derive Arbitrary for validated wrappers") {
        val errors = typeCheckErrors("""
          import neotype.*
          import neotype.interop.scalacheck.given
          import org.scalacheck.Arbitrary

          type NonEmpty = NonEmpty.Type
          object NonEmpty extends Newtype[String]:
            override inline def validate(value: String) = value.nonEmpty

          summon[Arbitrary[NonEmpty]]
        """)

        assertTrue(errors.nonEmpty)
      },
      test("creates validated newtype generators") {
        val valid   = gen[String, ValidatedNewtype](Gen.const("valid"))
        val invalid = gen[String, ValidatedNewtype](Gen.const(""))

        assertTrue(valid.sample.contains(ValidatedNewtype("valid")), invalid.sample.isEmpty)
      },
      test("creates validated subtype generators") {
        val valid   = gen[String, ValidatedSubtype](Gen.const("long enough value"))
        val invalid = gen[String, ValidatedSubtype](Gen.const("short"))

        assertTrue(valid.sample.contains(ValidatedSubtype("long enough value")), invalid.sample.isEmpty)
      }
    )
