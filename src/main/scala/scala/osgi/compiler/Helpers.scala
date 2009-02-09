package scala.osgi.compiler

trait Helpers {
   def safeDo[A](f : =>A) : Option[A] = {
     try {
       Some(f)
     } catch {
       case _ => 
         //TODo - Log?
         None
     }
   }
}
