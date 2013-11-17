import JaCoP.scala._

object main extends jacop {
  def main(args: Array[String]): Unit = {
//    val lundi		= 1
//	val mardi		= 2
//	val mercredi	= 3
//	val jeudi		= 4
//	val vendredi	= 5
//	
//	val _8h30	= 1
//	val _9h30	= 2
//	val _10h30	= 3
//	val _11h30	= 4
//	val _13h00	= 5
//	val _14h00	= 6
//	val _15h00	= 7
//	val _16h00	= 8
//	
//	val _017	= 1
//	val _019	= 2
	
	// variables
	val jour = IntVar("jour", 1, 5)
	val heure = IntVar("heure", 1, 8)
	val local = IntVar("local", 1, 2)
	val prof = IntVar("prof", 1, 2)
	
	// tableau de variables
	val vars = List(jour, heure, local, prof)
	
	// conditions
	//TODO
	
	
	// affichage et satisfaction
	def printSol(): Unit = {
	  for (v <- vars) print(v.id + " " + v.value + " ")
	  	println()
	}
	
	val result = satisfyAll(search(vars, first_fail, indomain_middle), printSol)
  }
}