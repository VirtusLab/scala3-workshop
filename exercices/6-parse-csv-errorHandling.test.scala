//> using toolkit default
package ex6

case class Entry(
    id: Int,
    name: String,
    sectionId: String,
    salary: Int,
    age: Byte
)

sealed trait DecodingFailure
case class NumberConversionFailure(input: String, field: String) extends DecodingFailure
case class IncorrectNumberOfFields(fields: List[String]) extends DecodingFailure
case class EmptyString(field: String) extends DecodingFailure

// Parse the string containg CSV data to List[Entry]
// Input might contain malformed data and might or might not contain header:
// id, name, sectionId, salary, age
//
// id, salary and age need to be valid integers
// name and sectionId cannot be empty
// Parser should ignore empty lines and header, for each other row it should return Either[DecodingFailure, Entry]
type DecoderResult[T] = Either[DecodingFailure, T]

def parseInput(input: String): List[DecoderResult[Entry]] = ???

class ParseCsvErrorHandlingSuite extends munit.FunSuite {
  val header = "id, name, sectionId, salary, age"
  test("decode empty strings") {
    val input =
      s"""$header
             |1,  ,foo,10,10
             |2,name, ,11,12
             |3,name,sectionId, 12, 13
          """.stripMargin
    assertEquals(
      parseInput(input),
      List(
        Left(EmptyString("name")),
        Left(EmptyString("sectionId")),
        Right(Entry(3, "name", "sectionId", 12, 13.toByte))
      )
    )
  }

  test("decode numbers") {
    val input =
      s"""$header
              |id,a,b,foo,10
              |2,a,b,small,12
              |3,a,b, 12, young
              |4,a,b, 13,30
           """.stripMargin
    assertEquals(
      parseInput(input),
      List(
        Left(NumberConversionFailure("id", field = "id")),
        Left(NumberConversionFailure("small", field = "salary")),
        Left(NumberConversionFailure("young", field = "age")),
        Right(Entry(4, "a", "b", 13, 30.toByte))
      )
    )
  }

  test("incorrect number of columns") {
    val input =
      s"""$header
                |1,name,section,1250
                |2,name,section,1250,20
                |3,name,section,1250,20,foobar
             """.stripMargin
    assertEquals(
      parseInput(input),
      List(
        Left(IncorrectNumberOfFields(List("1", "name", "section", "1250"))),
        Right(Entry(2, "name", "section", 1250, 20.toByte)),
        Left(
          IncorrectNumberOfFields(
            List("3", "name", "section", "1250", "20", "foobar")
          )
        )
      )
    )
  }

  test("decoding, no header") {
    val input = s"""
          |1,name,section,1250,20
          |""".stripMargin
    assertEquals(
      parseInput(input),
      List(
        Right(Entry(1, "name", "section", 1250, 20.toByte))
      )
    )
  }
}
