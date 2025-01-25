package sangria.marshalling.testkit

import sangria.marshalling._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

trait InputHandlingBehaviour {
  this: AnyWordSpec with Matchers =>

  def `AST-based input marshaller`[Raw](rm: ResultMarshaller)(implicit
      ti: ToInput[rm.Node, Raw]): Unit = {
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

  def `case class input marshaller`[Raw](rm: ResultMarshaller)(implicit
      ti: ToInput[Article, Raw]): Unit = {
    "handle undefined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(Article("Foo", None, None, Nil))

      verifyUndefined(raw, rm)(iu)
    }

    "handle deep undefined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(Article("Foo", None, None, List(Comment("bob", None))))

      verifyDeepUndefined(raw, rm)(iu)
    }

    "handle defined values (case class marshalling)" in {
      val (raw, iu) = ti.toInput(
        Article(
          "Foo",
          Some("foo bar and baz"),
          Some(List("culture", "nature")),
          List(Comment("bob", Some("first!")))))

      verifyDefined(raw, rm)(iu)
    }
  }

  def `AST-based input unmarshaller`[T](
      fi: FromInput[T])(implicit iu: InputUnmarshaller[T]): Unit = {
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

      res should be(Article("Foo", None, None, Nil))
    }

    "handle null values (case class unmarshalling)" in {
      val res = fi.fromResult(marshalNull(fi.marshaller))

      res should be(Article("Foo", None, None, List(Comment("bob", None), Comment("bob1", None))))
    }

    "handle defined values (case class unmarshalling)" in {
      val res = fi.fromResult(marshalDefined(fi.marshaller))

      res should be(
        Article(
          "Foo",
          Some("foo bar and baz"),
          Some(List("culture", "nature")),
          List(Comment("bob", Some("first!")))))
    }

    "throw exception on invalid input (case class unmarshalling)" in {
      an[InputParsingError] should be thrownBy fi.fromResult(fi.marshaller.mapNode(Seq.empty))
    }
  }

  private def marshalUndefined(rm: ResultMarshaller): rm.Node =
    rm.mapNode(
      Seq(
        "title" -> rm.scalarNode("Foo", "Test", Set.empty),
        "comments" -> rm.arrayNode(Vector.empty)
      ))

  private def verifyUndefined[T](res: T, m: ResultMarshaller)(implicit
      iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be(true)

    iu.getMapKeys(res) should contain("title").and(contain("comments"))
    iu.getMapValue(res, "title") should be(Some(m.scalarNode("Foo", "Test", Set.empty)))
    iu.getMapValue(res, "comments") should be(Some(m.arrayNode(Vector.empty)))

    assertPossibleNullNodes(res, m, "text" :: "tags" :: Nil)
  }

  private def verifyDeepUndefined[T](res: T, m: ResultMarshaller)(implicit
      iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be(true)

    iu.getMapKeys(res) should contain("title").and(contain("comments"))
    iu.getMapValue(res, "title") should be(Some(m.scalarNode("Foo", "Test", Set.empty)))

    assertPossibleNullNodes(res, m, "text" :: "tags" :: Nil)

    val Some(comments) = iu.getMapValue(res, "comments")

    iu.isListNode(comments) should be(true)

    val commentSeq = iu.getListValue(comments)

    commentSeq should have size 1

    iu.getMapValue(commentSeq(0), "author") should be(Some(m.scalarNode("bob", "Test", Set.empty)))

    assertPossibleNullNodes(commentSeq(0), m, "text" :: Nil)
  }

  def assertPossibleNullNodes[T](res: T, m: ResultMarshaller, names: Seq[String])(implicit
      iu: InputUnmarshaller[T]): Unit =
    names.foreach { name =>
      if (iu.getMapKeys(res).exists(_ == name))
        iu.getMapValue(res, name) should be(Some(m.nullNode))
    }

  private def marshalNull(rm: ResultMarshaller): rm.Node =
    rm.mapNode(
      Seq(
        "title" -> rm.scalarNode("Foo", "Test", Set.empty),
        "text" -> rm.nullNode,
        "tags" -> rm.nullNode,
        "comments" -> rm.arrayNode(Vector(
          rm.mapNode(List("author" -> rm.scalarNode("bob", "Test", Set.empty))),
          rm.mapNode(
            List("author" -> rm.scalarNode("bob1", "Test", Set.empty), "text" -> rm.nullNode))
        ))
      ))

  private def verifyNull[T](
      res: T,
      m: ResultMarshaller)(implicit iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be(true)

    iu.getMapValue(res, "title") should be(Some(m.scalarNode("Foo", "Test", Set.empty)))
    iu.getMapValue(res, "text") should be(Some(m.nullNode))
    iu.getMapValue(res, "tags") should be(Some(m.nullNode))
    iu.getMapValue(res, "comments") should be(
      Some(m.arrayNode(Vector(
        m.mapNode(Vector("author" -> m.scalarNode("bob", "Test", Set.empty))),
        m.mapNode(Vector("author" -> m.scalarNode("bob1", "Test", Set.empty), "text" -> m.nullNode))
      ))))
  }

  private def marshalDefined(rm: ResultMarshaller): rm.Node =
    rm.mapNode(
      Seq(
        "title" -> rm.scalarNode("Foo", "Test", Set.empty),
        "text" -> rm.scalarNode("foo bar and baz", "Test", Set.empty),
        "tags" -> rm.arrayNode(
          Vector(
            rm.scalarNode("culture", "Test", Set.empty),
            rm.scalarNode("nature", "Test", Set.empty))),
        "comments" -> rm.arrayNode(
          Vector(
            rm.mapNode(List(
              "author" -> rm.scalarNode("bob", "Test", Set.empty),
              "text" -> rm.scalarNode("first!", "Test", Set.empty)))
          ))
      ))

  private def verifyDefined[T](res: T, m: ResultMarshaller)(implicit
      iu: InputUnmarshaller[T]): Unit = {
    iu.isMapNode(res) should be(true)

    iu.getMapValue(res, "title") should be(Some(m.scalarNode("Foo", "Test", Set.empty)))
    iu.getMapValue(res, "text") should be(Some(m.scalarNode("foo bar and baz", "Test", Set.empty)))
    iu.getMapValue(res, "tags") should be(
      Some(
        m.arrayNode(Vector(
          m.scalarNode("culture", "Test", Set.empty),
          m.scalarNode("nature", "Test", Set.empty)))))
    iu.getMapValue(res, "comments") should be(
      Some(
        m.arrayNode(
          Vector(
            m.mapNode(Vector(
              "author" -> m.scalarNode("bob", "Test", Set.empty),
              "text" -> m.scalarNode("first!", "Test", Set.empty)))
          ))))
  }
}

case class Comment(author: String, text: Option[String])
case class Article(
    title: String,
    text: Option[String],
    tags: Option[List[String]],
    comments: List[Comment])
