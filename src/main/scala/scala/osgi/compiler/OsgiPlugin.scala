package scala.osgi.compiler

import tools.nsc.plugins._
import tools.nsc.backend.icode._
import tools.nsc._
import phases._
/** 
 * This plugin is used to enforce that only exported packages of other modules can be imported!
 */
class OsgiPlugin(val global : Global) extends Plugin {
  val name = "privateSetter"
  val description = "allows vars to have private setters"
  
  var allowedImports  = new java.util.HashSet[String]

  
  val infos = new CompileTimeInformation {
    def addPackageExport(pkg : String) : Unit = allowedImports.add(pkg)
    def addAllowedPackageImport(pkg:String) : Unit = allowedImports.add(pkg)
    def isPackageImportAllowed(pkg : String) : Boolean =  pkg.startsWith("java") || allowedImports.contains(pkg) || pkg.startsWith("scala")
  }
  
  val manifestChecker = new ManifestChecker(global, infos)  
  //TODO - make sure allowed imports is initialized before this is created...
  val importChecker = new ImportChecker(global, infos)
  //TODO - See if we can turn off components/phases based on input settings...
  val components = List[PluginComponent](manifestChecker,importChecker)  
  
  


  
  


}
