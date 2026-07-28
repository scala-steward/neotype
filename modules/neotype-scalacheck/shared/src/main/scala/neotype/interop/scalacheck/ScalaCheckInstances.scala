package neotype.interop.scalacheck

import neotype.*
import org.scalacheck.{Arbitrary, Gen}

/** Creates a generator that validates each underlying value before wrapping it.
  * Invalid values are discarded by ScalaCheck.
  */
def gen[A, B](underlying: Gen[A])(using wrappedType: WrappedType[A, B]): Gen[B] =
  underlying.flatMap { value =>
    wrappedType.make(value) match
      case Right(wrapped) => Gen.const(wrapped)
      case Left(_)        => Gen.fail
  }

given [A, B](using wrappedType: SimpleWrappedType[A, B], arbitrary: Arbitrary[A]): Arbitrary[B] =
  Arbitrary(wrappedType.unsafeMakeF(arbitrary.arbitrary))
