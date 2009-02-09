package scala.osgi.manifest

import org.junit._
import Assert._

class TestManifestReader {

  @Test
  def mustReadManifest() {    
    //val reader = new Object with ManifestIo {}
    val result = ManifestIo.readManifest(getClass.getResourceAsStream("MANIFEST.MF"))
    assertEquals("Failed to read manifest!", "1.0",result.version)
    assertEquals("Failed to read manifest!", "2", result("Bundle-ManifestVersion"))
    assertEquals("Failed to read manifest!", "Scala Distribution", result("Bundle-Name"))
    
    ()
  }
}
