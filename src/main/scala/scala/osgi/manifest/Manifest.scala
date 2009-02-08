package scala.osgi.manifest

//TODO - Handle manifest sections...
/**
 * This class represents a parsed Manifest file from a java JAR/WAR/EAR package.
 */
class Manifest(val version : String, values_ : Map[String,String]) {
  private var values = values_  
  //TODO - handle sections?
  def apply(name : String) = values(name)
  
  def set(name : String, value : String) {
    values = values + ((name, value)) 
  }
  
  
  private def makeStringForKey(key : String, value : String) = {
    //TODO -Limit width to 80 chars before newlining
    key + ": " + value + "\n" 
  }
  
  override def toString = {
    val sepChar = ":"
    val eol = "\n"
    val headers = values.map {
      case (x,y) => makeStringForKey(x,y)
    }.mkString(eol)
    //TODO - Convert name-value pairs into lines
    "Manifest-Version" + sepChar + " " + version + eol + headers
  }
}
