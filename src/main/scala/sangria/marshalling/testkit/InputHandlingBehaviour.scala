package sangria.marshalling.testkit

import org.scalatest.{WordSpec, Matchers}
import sangria.marshalling._

trait InputHandlingBehaviour {
  this: WordSpec with Matchers ⇒

  case class Comment(author: String, text: Option[String])
  case class Article(title: String, text: Option[String], tags: Option[List[String]], comments: List[Comment])

  def `AST-based input marshaller`[Raw](rm: ResultMarshaller)(implicit ti: ToInput[rm.Node, Raw]): Unit = {
    "handle undefined values (ast-based marshalling)" in {
      val marshaled = marshalUndefined(rm)
      val (raw, iu) = ti.toInput(marshaled)

      verifyUndefined(raw, rm)(iu)
    }

    "handle null values (ast-based marshalling)" in {
      val marshaled = marshalNull(rm)
      val (raw, iu) = ti.toInput(marshaled)

      verifyNull(raw, rm)(iu)
    }

    "handle defined values (ast-based marshalling)" in {
      val marshaled = marshalDefined(rm)
      val (raw, iu) = ti.toInput(marshaled)

      verifyDefined(raw, rm)(iu)
    }
  }

  def `case class input marshaller`[Raw](rm: ResultMarshaller)(implicit ti: ToInput[Article, Raw]): Unit = {
    "handle undefined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(Article("Foo", None, None, Nil))

      verifyUndefined(raw, rm)(iu)
    }

    "handle deep undefined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(Article("Foo", None, None, List(Comment("bob", None))))

      verifyDeepUndefined(raw, rm)(iu)
    }

    "handle defined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(Article(
        "Foo",
        Some("foo bar and baz"),
        Some(List("culture", "nature")),
        List(Comment("bob", Some("first!")))))

      verifyDefined(raw, rm)(iu)
    }
  }

  def `AST-based input unmarshaller`[T](fi: FromInput[T])(implicit iu: InputUnmarshaller[T]): Unit = {
    "handle undefined values (ast-based unmarshalling)" in {
      val m = fi.marshaller
      val res = fi.fromResult(marshalUndefined(fi.marshaller))

      verifyUndefined(res, m)
    }

    "handle null values (ast-based unmarshalling)" in {
      val m = fi.marshaller
      val res = fi.fromResult(marshalNull(fi.marshaller))

      verifyNull(res, m)
    }

    "handle defined values (ast-based unmarshalling)" in {
      val m = fi.marshaller
      val res = fi.fromResult(marshalDefined(fi.marshaller))

      verifyDefined(res, m)
    }
  }

  def `case class input unmarshaller`(implicit fi: FromInput[Article]): Unit = {
    "handle undefined values (case class unmarshalling)" in {
      val res = fi.fromResult(marshalUndefined(fi.marshaller))

      res should be (Article("Foo", None, None, Nil))
    }

    "handle null values (case class unmarshalling)" in {
      val res = fi.fromResult(marshalNull(fi.marshaller))

      res should be (Article("Foo", None, None, List(Comment("bob", None), Comment("bob1", None))))
    }

    "handle defined values (case class unmarshalling)" in {
      val res = fi.fromResult(marshalDefined(fi.marshaller))

      res should be (Article(
        "Foo",
        Some("foo bar and baz"),
        Some(List("culture", "nature")),
        List(Comment("bob", Some("first!")))))
    }

    "throw exception on invalid input (case class unmarshalling)" in {
      an [InputParsingError] should be thrownBy fi.fromResult(fi.marshaller.mapNode(Seq.empty))
    }
  }

  private def marshalUndefined(rm: ResultMarshaller): rm.Node =
    rm.mapNode(Seq(
      "title" → rm.stringNode("Foo"),
      "comments" → rm.arrayNode(Vector.empty)
    ))

  private def verifyUndefined[T](res: T, m: ResultMarshaller)(implicit iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be (true)

    iu.getMapKeys(res) should (have(size(2)) and contain("title") and contain("comments"))
    iu.getMapValue(res, "title") should be (Some(m.stringNode("Foo")))
    iu.getMapValue(res, "comments") should be (Some(m.arrayNode(Vector.empty)))
  }
  private def verifyDeepUndefined[T](res: T, m: ResultMarshaller)(implicit iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be (true)

    iu.getMapKeys(res) should (have(size(2)) and contain("title") and contain("comments"))
    iu.getMapValue(res, "title") should be (Some(m.stringNode("Foo")))
    iu.getMapValue(res, "comments") should be (Some(m.arrayNode(Vector(
      m.mapNode(Vector("author" → m.stringNode("bob")))
    ))))
  }

  private def marshalNull(rm: ResultMarshaller): rm.Node =
    rm.mapNode(Seq(
      "title" → rm.stringNode("Foo"),
      "text" → rm.nullNode,
      "tags" → rm.nullNode,
      "comments" → rm.arrayNode(Vector(
        rm.mapNode(List(
          "author" → rm.stringNode("bob"))),
        rm.mapNode(List(
          "author" → rm.stringNode("bob1"),
          "text" → rm.nullNode))
      ))
    ))

  private def verifyNull[T](res: T, m: ResultMarshaller)(implicit iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be (true)

    iu.getMapValue(res, "title") should be (Some(m.stringNode("Foo")))
    iu.getMapValue(res, "text") should be (Some(m.nullNode))
    iu.getMapValue(res, "tags") should be (Some(m.nullNode))
    iu.getMapValue(res, "comments") should be (Some(m.arrayNode(Vector(
      m.mapNode(Vector("author" → m.stringNode("bob"))),
      m.mapNode(Vector("author" → m.stringNode("bob1"), "text" → m.nullNode))
    ))))
  }

  private def marshalDefined(rm: ResultMarshaller): rm.Node =
    rm.mapNode(Seq(
      "title" → rm.stringNode("Foo"),
      "text" → rm.stringNode("foo bar and baz"),
      "tags" → rm.arrayNode(Vector(rm.stringNode("culture"), rm.stringNode("nature"))),
      "comments" → rm.arrayNode(Vector(
        rm.mapNode(List(
          "author" → rm.stringNode("bob"),
          "text" → rm.stringNode("first!")))
      ))
    ))

  private def verifyDefined[T](res: T, m: ResultMarshaller)(implicit iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be (true)

    iu.getMapValue(res, "title") should be (Some(m.stringNode("Foo")))
    iu.getMapValue(res, "text") should be (Some(m.stringNode("foo bar and baz")))
    iu.getMapValue(res, "tags") should be (Some(m.arrayNode(Vector(m.stringNode("culture"), m.stringNode("nature")))))
    iu.getMapValue(res, "comments") should be (Some(m.arrayNode(Vector(
      m.mapNode(Vector("author" → m.stringNode("bob"), "text" → m.stringNode("first!")))
    ))))
  }
}
