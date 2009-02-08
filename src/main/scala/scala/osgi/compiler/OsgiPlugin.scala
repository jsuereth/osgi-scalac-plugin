package scala.osgi.compiler

import tools.nsc.plugins._
import tools.nsc.backend.icode._
import tools.nsc._

class OsgiPlugin(val global : Global) extends Plugin {
  val name = "privateSetter"
  val description = "allows vars to have private setters"
  val components = List[PluginComponent]()
  
}
