import JaCoP.scala._

object main extends jacop {
  def main(args: Array[String]): Unit = {
    // association valeurs => noms
    val jourNoms  = "Lundi" :: "Mardi" :: "Mercredi" :: "Jeudi" :: "Vendredi" :: Nil
    val heureNoms = "8h30" :: "9h30" :: "10h30" :: "11h30" :: "13h00" :: "14h00" :: "15h00" :: "16h00" :: Nil
    val localNoms = "017" :: "019" :: Nil
    val profNoms  = "Donatien" :: "Brigitte" :: Nil
	
	// variables
    val jours  = for(i <- List.range(0, jourNoms.length)) yield IntVar(jourNoms(i), 1, jourNoms.length)
    val heures = for(i <- List.range(0, heureNoms.length)) yield IntVar(heureNoms(i), 1, heureNoms.length)
    val locaux = for(i <- List.range(0, localNoms.length)) yield IntVar(localNoms(i), 1, localNoms.length)
    val profs  = for(i <- List.range(0, profNoms.length)) yield IntVar(profNoms(i), 1, profNoms.length)
    
    // conditions
    alldifferent(jours)
    alldifferent(heures)
    alldifferent(locaux)
    alldifferent(profs)
	
	// tableau de variables
    val vars = jours ::: heures ::: locaux ::: profs
	
	/* donatien donne pas cours le mercredi
	
	if (prof == donatien)
	  jour != mercredi
	  
	  donatien == prof;
	  jour != mercredi;
	  prof == donatien -> jour != mercredi
	*/
	// conditions
	//TODO
	
	// affichage et satisfaction
	def printSol(): Unit = {
	  for (v <- vars) {
	    var result = Map[Int, (String, String, String)]();
	    
	      for (v <- jours) {
	        result += (v.value -> (v.id, "", ""))
	      }
	      for (v <- heures) {
	        result += (v.value -> (result(v.value)._1, v.id, ""))
	      }
	      for (v <- locaux) {
	        result += (v.value -> (result(v.value)._1, result(v.value)._2, v.id))
	      }
	
	      for (v <- result) {
	        println(v._2._1 + " " + v._2._2 + " " + v._2._3)
	      }
	  }
	  
	  println()
	}
	
	val result = satisfyAll(search(vars, first_fail, indomain_middle), printSol)
  }
}