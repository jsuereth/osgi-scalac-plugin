package scala.osgi.manifest

import org.junit._
import Assert._

class TestManifestParser extends ManifestParser {

  
  
  
  def parserHelper[A](parser:Parser[A])(input : String)(checker : PartialFunction[A,Unit]) {

    def cleanUpReason(reason : String) = reason.toCharArray.flatMap(_ match {
	        case '\\' => List('\\','\\') 
		    case '\0' => List('\\','0')
		    case '\n' => List('\\','n')
		    case '\r' => List('\\','r')
		    case x => List(x)
		  }).mkString("")
    
    parseAll(parser, input) match {
      case Success(value, _) =>
        if(checker.isDefinedAt(value)) {
          checker(value)
        } else {
          fail("Failed to parse input!")
        }        
      case Error(reason, input) => 
        
        fail("At line: " + input.pos.line + " - " + cleanUpReason(reason))
      case Failure(reason, input) => 
        
        fail("At line: " + input.pos.line + " - " + cleanUpReason(reason))
    }
    
  }
  @Test
  def mustParseHeaders() {
    val helper = parserHelper(header) _
    
    helper("""DummyKey: DummyValue
""") {
        case (key,value) =>
            assertEquals("DummyKey", key)
            assertEquals("DummyValue", value)
     }
    helper("""DummyKey: 
 DummyValue
""") {
        case (key,value) =>
            assertEquals("DummyKey", key)
            assertEquals("DummyValue", value)
     }    
  }
    @Test
  def mustParseKeys() {
    val helper = parserHelper(prop_name) _
    
    helper("""DummyKey""") {
        case key =>
            assertEquals("DummyKey", key);
            ()
     }
  }
  @Test
  def mustParseEol() {
    val helper = parserHelper(eol) _
    helper("\r\n") {
      case x => //Ignore
    }
    helper("\r") {
      case x => //Ignore
    }
    helper("\n") {
      case x => //Ignore
    }
  }
}
