package scala.osgi.manifest

import java.io._
/**
 * This trait helps in reading manifest files and parsing their contents
 */
object ManifestIo extends ManifestParser {
  
  /** Reads in a manifest file from the input stream*/
  def readManifest(in : InputStream) = {    
     parse(simple_manifest, new InputStreamReader(in)) match {
	    case Success(manifest,_) => manifest
        case Error(reason,source) => error(source.pos.line + ":"+ reason)
	    case Failure(reason,source) =>
	      val eReason = reason.toCharArray.flatMap(_ match {
	        case '\\' => List('\\','\\') 
		    case '\0' => List('\\','0')
		    case '\n' => List('\\','n')
		    case '\r' => List('\\','r')
		    case x => List(x)
		  }).mkString("")
	      error(source.pos.line + ":"+eReason) 
	  }
  }
  
  def readOSGiManifest(in : InputStream) : Either[Manifest, OSGIManifest]= {
    readManifest(in) match {
      case OSGiManifestType(manifest) => Right(manifest)
      case x => Left(x) 
    }
  }
  
  def writeManifest(manifest : Map[String,String], out : PrintStream) = {
    out.println(manifest.toString)
  }
}
