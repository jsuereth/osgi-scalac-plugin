package scala.osgi.manifest


import util.parsing._
import combinator._
import input._
/**
 * Parser for jar manifests, grammar from here: http://java.sun.com/j2se/1.3/docs/guide/jar/jar.html
 */
trait ManifestParser extends RegexParsers {
  
  override protected val whiteSpace = """[\u0000]""".r
    
  def simple_manifest = version_info ~ rep(header)  ^^ {
    case version ~ properties =>
      Console.println("Creating Manifest with keys: " + properties.map({case (x,y) => x + "=" + y}).mkString(", "))
      
      new Manifest(version, Map(properties : _*))
  }
  
  def version_info = ("Manifest-Version" ~ property_sep) ~> version_number <~ eol
  
  def version_number = space ~> rep1sep(integerValue, ".") ^^ {
    case first  => first.mkString(".")
  }
  
  def property_sep : Parser[String]= ":"
  def integerValue :Parser[String] = """[\d]+""".r
  
  def eol = ("[\r]".r ~ opt("[\n]".r)) | "[\n]".r
  
  def header = prop_name ~ (property_sep ~> prop_value) ^^ { case x ~ y => (x,y) }
  def prop_name = "\\w".r ~ rep(header_char) ^^ { case x ~ y => x + y.mkString("")}
  
  def prop_value = space ~> rep(other_char) ~ (eol ~> rep(value_continuation)) ^^ {
    case firstline ~ otherlines => firstline.mkString("") + otherlines.mkString("")
      
  }
  def value_continuation = space ~> rep(other_char) <~ eol ^^ {
    case value => value mkString ""
  }
  //TODO - This might need to be strict...
  def space : Parser[String] = "\\s+".r
  def header_char = "\\w".r | '-' | '_'
  def other_char :Parser[String] = """[^\r\n\u0000]""".r
}
