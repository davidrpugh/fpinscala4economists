package parsing


trait JSON


object JSON {

  def parser[E, Parser[+_]](ps: Parsers[E, Parser]): Parser[JSON] = {
    import ps.{string => _, _}
    implicit def toToken(s: String) = token(ps.string(s))


    def array: Parser[JArray] = {
      surround("[", "]")(value.sep(",").map(vs => JArray(vs.toIndexedSeq))).scope("array")
    }

    def obj = surround("{", "}")(keyValue.sep(",").map(kvs => JObject(kvs.toMap))).scope("object")

    def keyValue = escapedQuoted ** (skipL(":", value))

    def literal = scope("literal") {
      "null".as(JNull) |
      double.map(v => JNumber(v)) |
      escapedQuoted.map(v => JString(v)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }

    def value: Parser[JSON] = {
      literal | obj | array
    }

    root(skipL(whitespace, obj | array))

  }


  /* Internal JSON ADT */
  private case object JNull extends JSON
  private case class JNumber(get: Double) extends JSON
  private case class JString(get: String) extends JSON
  private case class JBool(get: Boolean) extends JSON
  private case class JArray(get: IndexedSeq[JSON]) extends JSON
  private case class JObject(get: Map[String, JSON]) extends JSON

}
