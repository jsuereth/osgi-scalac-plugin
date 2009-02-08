package scala.osgi.compiler.phases

import tools.nsc
import nsc._
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

import java.io.File._

class ReadBundleManifestsPhase(prev : Phase, global : Global) extends Phase(prev) {
   import global._  
   override def name = "read-osgi-manifests"
     
   /**
    * Called when our plugin is running.
    */
   override def run {
     val classpath = global.settings.classpath.value
     
     val paths = classpath.split(pathSeparator)
     
     ()
   }
}
