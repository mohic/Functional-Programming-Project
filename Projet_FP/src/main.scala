import JaCoP.scala._

object main extends jacop {
  def main(args: Array[String]): Unit = {
    // association valeurs => noms
    val jourNoms  = "Lundi" :: "Mardi" :: "Mercredi" :: "Jeudi" :: "Vendredi" :: Nil
    val heureNoms = "8h30" :: "9h30" :: "10h30" :: "11h30" :: "13h00" :: "14h00" :: "15h00" :: "16h00" :: Nil
    val localNoms = "017" :: "019" :: Nil
    
    val profNoms  = "Donatien" :: "Brigitte" :: Nil
    val coursNoms = "Algorithme" :: "Calcul numérique" :: "Scala" :: Nil

    // variables
    val jour = IntVar("jour", 1, jourNoms.length)
    val heure = IntVar("heure", 1, heureNoms.length)
    val local = IntVar("local", 1, localNoms.length)
    val jhl = jour * 100 + heure * 10 + local
    
    val prof = IntVar("prof", 1, profNoms.length)
    val cours = IntVar("cours", 1, coursNoms.length)
    
    var b = BoolVar("donatienPasCoursMecredi")
    
//    OR(AND(b #= true , prof #= 1 , cours #= 1),
//       AND(b #= false, prof #\= 1, cours #\= 1))
    
    // tableau de variables
    val vars = List(jhl, prof, cours)

    // conditions
    //(prof == 1) -> (jour != 3) // donatien ne donne pas cours le mercredi

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
          print("\t" + v.id + " = ")
          
          if (v.id == prof.id) {
        	  print(profNoms(v.value() - 1))
          } else if (v.id == cours.id) {
        	  print(coursNoms(v.value() - 1))
          } else {
            print(v.value())
          }
        }

        print(" ")

        println()
      }
    }

    val result = satisfyAll(search(vars, first_fail, indomain_min), printSol)
  }
}