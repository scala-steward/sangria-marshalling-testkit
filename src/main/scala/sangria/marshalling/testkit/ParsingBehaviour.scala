package sangria.marshalling.testkit

import sangria.marshalling.{InputParser, InputUnmarshaller}

import scala.util.Success
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait ParsingBehaviour {
  this: AnyWordSpec with Matchers =>

  def `input parser`[T: InputUnmarshaller: InputParser](testSubjects: ParseTestSubjects): Unit = {
    val iu = implicitly[InputUnmarshaller[T]]
    val parser = implicitly[InputParser[T]]

    "parse simple string values in input" in {
      val Success(parsed) = parser.parse(testSubjects.simpleString)

      iu.isDefined(parsed) should be(true)
      iu.isScalarNode(parsed) should be(true)
      iu.getScalaScalarValue(parsed) should be("bar")
    }

    "parse simple int values in input" in {
      val Success(parsed) = parser.parse(testSubjects.simpleInt)

      iu.isDefined(parsed) should be(true)
      iu.isScalarNode(parsed) should be(true)
      iu.getScalaScalarValue(parsed) should be(12345)
    }

    "parse simple null values in input" in {
      val Success(parsed) = parser.parse(testSubjects.simpleNull)

      iu.isScalarNode(parsed) should be(false)
      iu.isDefined(parsed) should be(false)
    }

    "parse list values in input" in {
      val Success(parsed) = parser.parse(testSubjects.list)

      iu.isDefined(parsed) should be(true)
      iu.isListNode(parsed) should be(true)

      val list = iu.getListValue(parsed)

      list should have size 5
      iu.getScalaScalarValue(list(0)) should be("bar")
      iu.getScalaScalarValue(list(1)) should be(1)
      iu.isDefined(list(2)) should be(false)
      iu.getScalaScalarValue(list(3)) should be(true: Any)
      iu.isListNode(list(4)) should be(true)

      val nested = iu.getListValue(list(4))

      nested should have size 3
      iu.getScalaScalarValue(nested(0)) should be(1)
      iu.getScalaScalarValue(nested(1)) should be(2)
      iu.getScalaScalarValue(nested(2)) should be(3)
    }

    "parse complex values in input" in {
      val Success(parsed) = parser.parse(testSubjects.complex)

      iu.isDefined(parsed) should be(true)
      iu.isMapNode(parsed) should be(true)
      iu.getMapKeys(parsed).toSet should be(Set("a", "b"))

      // complex = "{a: [null, 123, [{foo: \"bar\"}]], b: {c: true, d: null}}",

      val a = iu.getMapValue(parsed, "a").get
      val b = iu.getMapValue(parsed, "b").get

      iu.isListNode(a) should be(true)

      val alist = iu.getListValue(a)

      alist should have size 3
      iu.isDefined(alist(0)) should be(false)
      iu.getScalaScalarValue(alist(1)) should be(123)
      iu.isListNode(alist(2)) should be(true)

      val nested = iu.getListValue(alist(2))

      nested should have size 1
      iu.isMapNode(nested(0)) should be(true)
      iu.getMapKeys(nested(0)).toSet should be(Set("foo"))

      val foo = iu.getMapValue(nested(0), "foo").get

      iu.getScalaScalarValue(foo) should be("bar")

      iu.isMapNode(b) should be(true)
      iu.getMapKeys(b).toSet should be(Set("c", "d"))

      val c = iu.getMapValue(b, "c").get
      val d = iu.getMapValue(b, "d").get

      iu.getScalaScalarValue(c) should be(true: Any)
      iu.isDefined(d) should be(false)
    }

    "result in failure in case of syntax errors" in
      testSubjects.syntaxError.foreach { broken =>
        val result = parser.parse(broken)

        result.isFailure should be(true)
      }
  }

  case class ParseTestSubjects(
      /** Following tests will expect input equivalent to following json:
        *
        * {{{
        *   {a: [null, 123, [{foo: "bar"}]], b: {c: true, d: null}}
        * }}}
        */
      complex: String,

      /** Following tests will expect input equivalent to following json:
        *
        * {{{
        *   "bar"
        * }}}
        */
      simpleString: String,

      /** Following tests will expect input equivalent to following json:
        *
        * {{{
        *   12345
        * }}}
        */
      simpleInt: String,

      /** Following tests will expect input equivalent to following json:
        *
        * {{{
        *   null
        * }}}
        */
      simpleNull: String,

      /** Following tests will expect input equivalent to following json:
        *
        * {{{
        *   ["bar", 1, null, true, [1, 2, 3]]
        * }}}
        */
      list: String,

      /** Following tests should contain a syntax error of some kind
        */
      syntaxError: List[String]
  )
}
