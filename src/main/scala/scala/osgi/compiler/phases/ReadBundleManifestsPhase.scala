package scala.osgi.compiler.phases

import tools.nsc
import nsc._
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import java.io.File._

import util._
import ClassPath._
import manifest._

/**
 * This plugin reads manifests
 */
class ReadBundleManifestsPhase(prev : Phase, global : Global, allowedImportsSetter : collection.Set[String] => Unit) extends Phase(prev) with Helpers {
   import global._
   import global.classPath0._
   
   override def name = "read-osgi-manifests"
   
   /**
    * Called when our plugin is running.
    */
   override def run {
     val classPath = global.classPath    
     val manifests = classPath.entries flatMap {       
       case lib : Library => 
         pullManiestFromLibrary(lib) match {
           case Some(m) => List(m)
           case None => Nil
         }
       case x => Nil
     } 
     Console.println("Found: " + manifests.length + " manifets")     
     allowedImportsSetter(getAllowedImports(manifests))
   }
   
   private def getAllowedImports(manifests : Seq[Manifest]) = {
     var importsAllowed : collection.immutable.Set[String] = collection.immutable.Set()
     for(manifest <- manifests) {
       import OSGiManifestType._       
       importsAllowed = importsAllowed ++ (safeDo(manifest.getExportedPackages).getOrElse(List()))
     }
     importsAllowed
   }
   
   private def pullManiestFromLibrary(lib : Library) :Option[Manifest] = safeDo {     
       val dir = lib.location.lookupPath("META-INF", true)
       val manifestfile = dir.lookupName("MANIFEST.MF", false)
       ManifestIo.readManifest(manifestfile.input)
   }
}
