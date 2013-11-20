import JaCoP.scala._

object main extends jacop {
  def main(args: Array[String]): Unit = {
    // association valeurs => noms
    val jourNoms  = "Lundi" :: "Mardi" :: "Mercredi" :: "Jeudi" :: "Vendredi" :: Nil
    val heureNoms = "8h30" :: "9h30" :: "10h30" :: "11h30" :: "13h00" :: "14h00" :: "15h00" :: "16h00" :: Nil
    val localNoms = "017" :: "019" :: Nil
    val profNoms  = "Donatien" :: "Brigitte" :: Nil
	
	// variables
	val jour  = IntVar("jour", 1, 5)
	val heure = IntVar("heure", 1, 8)
	val local = IntVar("local", 1, 2)
	val prof  = IntVar("prof", 1, 2)
	
	// tableau de variables
	val vars = List(jour, heure, local, prof)
	
	// conditions
	//TODO
	
	// affichage et satisfaction
	def printSol(): Unit = {
	  for (v <- vars) {
	    print(v.id + " = ")
	    
	    v.id match {
	      case "jour"  => print(jourNoms(v.value() - 1))
	      case "heure" => print(heureNoms(v.value() - 1))
	      case "local" => print(localNoms(v.value() - 1))
	      case "prof" => print(profNoms(v.value() - 1))
	    }
	    
	    print(" ")
	  }
	  
	  println()
	}
	
	val result = satisfyAll(search(vars, first_fail, indomain_middle), printSol)
  }
}