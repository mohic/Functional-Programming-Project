import JaCoP.scala._

object AncienMain extends jacop {
  def main(args: Array[String]): Unit = {
    // association valeurs => noms
    val jourNoms = "Lundi" :: "Mardi" :: "Mercredi" :: "Jeudi" :: "Vendredi" :: Nil
    val heureNoms = "8h30" :: "9h30" :: "10h30" :: "11h30" :: "13h00" :: "14h00" :: "15h00" :: "16h00" :: Nil
    val localNoms = "017" :: "019" :: Nil
    val profNoms = "Donatien" :: "Brigitte" :: Nil

    // variables
    val jour = IntVar("jour", 1, 5)
    val heure = IntVar("heure", 1, 8)
    val local = IntVar("local", 1, 2)
    val jhl = jour * 100 + heure * 10 + local
    val prof = IntVar("prof", 1, 2)

    // tableau de variables
    val vars = List(jhl, prof)

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
        if (v.id == jhl.id) {
          var i = v.value() / 100
          print("jour = " + jourNoms(i - 1) + " ")

          i = v.value() - (i * 100)
          print("heure = " + heureNoms((i / 10) - 1) + " ")

          print("local = " + localNoms((v.value() % 10) - 1) + " ")
        } else {
          print(v.id + " = ")
          print(profNoms(v.value() - 1))
        }

        print(" ")

        println()
      }

      val result = satisfyAll(search(vars, first_fail, indomain_middle), printSol)
    }
  }
}