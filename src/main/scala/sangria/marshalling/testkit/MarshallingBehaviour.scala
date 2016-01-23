package sangria.marshalling.testkit

import org.scalatest.{WordSpec, Matchers}
import sangria.marshalling.{ScalaInput, InputUnmarshaller, ResultMarshaller}

import sangria.marshalling.MarshallingUtil._
import sangria.util.tag._

trait MarshallingBehaviour {
  this: WordSpec with Matchers ⇒

  def `value (un)marshaller`[T](rm: ResultMarshaller)(implicit iu: InputUnmarshaller[rm.Node]): Unit = {
    "(un)marshal boolean scalar values" in {
      val marshaled = rm.booleanNode(true)

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      scalaScalar should be (true: Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal int scalar values" in {
      val marshaled = rm.intNode(123)

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      iu.getScalaScalarValue(marshaled) should be (123: Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal big int scalar values" in {
      val marshaled = rm.bigIntNode(BigInt("12323432432432"))

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      iu.getScalaScalarValue(marshaled) should be (BigInt("12323432432432"): Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal float scalar values" in {
      val marshaled = rm.floatNode(123.456D)

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      iu.getScalaScalarValue(marshaled) should be (123.456D: Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal big decimal scalar values" in {
      val marshaled = rm.bigDecimalNode(BigDecimal("12323432432432.2435454354543"))

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      iu.getScalaScalarValue(marshaled) should be (BigDecimal("12323432432432.2435454354543"): Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal string scalar values" in {
      val marshaled = rm.stringNode("Hello world")

      val scalar = iu.getScalarValue(marshaled)
      val scalaScalar = iu.getScalaScalarValue(marshaled)

      scalaScalar should be ("Hello world": Any)

      if (scalar != scalaScalar)
        scalar should be (marshaled)

      iu.isScalarNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isEnumNode(marshaled) should be (scalar == scalaScalar)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal nulls" in {
      val marshaled = rm.nullNode

      iu.isDefined(marshaled) should be (false)

      iu.isScalarNode(marshaled) should be (false)
      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
      iu.isListNode(marshaled) should be (false)
    }

    "(un)marshal list values" in {
      import sangria.marshalling.scalaMarshalling._

      val map = rm.mapNode(Vector("a" → rm.intNode(1)))
      val seq = Vector(map, rm.nullNode, rm.stringNode("ABC"), rm.arrayNode(Vector.empty))
      val marshaled = rm.arrayNode(seq)

      marshaled.convertMarshaled[Any @@ ScalaInput] should be (
        Vector(Map("a" → 1), null, "ABC", Vector.empty))

      iu.getListValue(marshaled) should be (seq)

      iu.isListNode(marshaled) should be (true)
      iu.isDefined(marshaled) should be (true)

      iu.isScalarNode(marshaled) should be (false)
      iu.isEnumNode(marshaled) should be (false)
      iu.isVariableNode(marshaled) should be (false)
      iu.isMapNode(marshaled) should be (false)
    }

    "(un)marshal map values" in {
      import sangria.marshalling.scalaMarshalling._

      val map = rm.mapNode(Vector("a" → rm.intNode(1)))
      val seq = rm.arrayNode(Vector(map, rm.nullNode, rm.stringNode("ABC"), rm.arrayNode(Vector.empty)))

      val marshaled1 = rm.mapNode(Vector("first" → seq, "second" → rm.nullNode))
      val marshaled2 = rm.addMapNodeElem(rm.addMapNodeElem(rm.emptyMapNode, "first", seq, false), "second", rm.nullNode, false)
      val marshaled3 = rm.addMapNodeElem(rm.addMapNodeElem(rm.emptyMapNode, "first", seq, true), "second", rm.nullNode, true)

      List(marshaled1, marshaled2, marshaled3) foreach { marshaled ⇒
        marshaled.convertMarshaled[Any @@ ScalaInput] should be (
          Map(
            "first" → Vector(Map("a" → 1), null, "ABC", Vector.empty),
            "second" → null))

        iu.isMapNode(marshaled) should be (true)
        iu.isDefined(marshaled) should be (true)

        iu.getMapKeys(marshaled) should (have(size(2)) and contain("first") and contain("second"))
        iu.getMapValue(marshaled, "first") should be (Some(seq))
        iu.getMapValue(marshaled, "second") should be (Some(rm.nullNode))
        iu.getMapValue(marshaled, "non-existing") should be (None)

        iu.getRootMapValue(marshaled, "first") should be (Some(seq))
        iu.getRootMapValue(marshaled, "non-existing") should be (None)

        iu.isListNode(marshaled) should be (false)
        iu.isScalarNode(marshaled) should be (false)
        iu.isEnumNode(marshaled) should be (false)
        iu.isVariableNode(marshaled) should be (false)
      }
    }

    "marshal optional list values" in {
      rm.optionalArrayNodeValue(Some(rm.intNode(123))) should be (rm.intNode(123))
      rm.optionalArrayNodeValue(Some(rm.nullNode)) should be (rm.nullNode)
      rm.optionalArrayNodeValue(None) should be (rm.nullNode)
    }
  }
}
