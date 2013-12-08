import JaCoP.scala._
import JaCoP.constraints.Sum

object main extends jacop {
  def main(args: Array[String]) {
    
    // liste des jours de la semaine
    val jours  = List("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")
    val heures = List("8h30", "9h30", "10h30", "11h30", "13h00", "14h00", "15h00", "16h00")
    val locaux = List("017", "019")
    
    // génération de la grille horaire
    val jhl = for (j <- List.range(0, jours.length);
    			   h <- List.range(0, heures.length);
    			   l <- List.range(0, locaux.length))
    	yield jours(j) + '_' + heures(h) + '_' + locaux(l)
    
    // liste des profs
    val prof  = List("AUCUN", "Donatien", "Brigitte", "Bernard", "Emmeline", "Gilles")
    
    // liste des cours
    val cours = List("AUCUN", "Algorithme", "Scala", "IOO", "BI", "ABC")
    
    // listes de IntVar pour indiquer des contraintes
    val jhl_prof  = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_prof_" + i, 1, prof.length)
    val jhl_cours = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_cours_" + i, 1, cours.length)
    
    // ******** functions
    
    /**
     * Obtenir l'index d'un jour de la semaine à une heure particulière et dans un local bien précis
     * @params jour Le jour de la semaine. "Lundi", "Mardi", ...
     * @params heure L'heure de la semaine. "8h30", "9h30", ...
     * @params local Le local. "017", "019", ...
     * @return L'index du jour dans jhl à partir de 1. 0 si pas trouvé
     * */
    def getJourHeureLocal(jour: String, heure: String, local: String): Int = {
      val s_jhl = jour + '_' + heure + '_' + local
      
      jhl.indexOf(s_jhl) + 1
    }
    
    /**
     * Obtenir l'index d'un professeur
     * @params professeur Le nom du professeur
     * @return L'index du professeur à partir de 1. 0 si pas trouvé
     * */
    def getProfesseur(professeur: String): Int = {
      prof.indexOf(professeur) + 1
    }
    
    /**
     * Obtenir l'index d'un cours
     * @params nomCours Le nom du cours
     * @return L'index du cours à partir de 1. 0 si pas trouvé
     * */
    def getCours(nomCours: String): Int = {
      cours.indexOf(nomCours) + 1
    }
    
    /**
     * Définit une contrainte disant que le prof ne donne pas cours tel jour de la semaine
     * @params prof Le nom du professeur
     * @params jour Le jour de la semaine
     * */
    def profDonnePasCoursJour(prof: String, jour: String): Unit = {
      val j = jours.indexOf(jour)
      
      for (i <- 0 to (heures.length * locaux.length - 1)) {
        jhl_prof(i + (j * heures.length * locaux.length)) #\= getProfesseur(prof)
      }
    }
    
    /**
     * Définit une contrainte disant que le professeur donne un certain cours durant maximum autant d'heures
     * @params prof Le nom du professeur
     * @params cours Le nom du cours
     * @params heuresMax Le nombre d'heures maximum
     * */
    def profDonneCours(prof: String, cours: String, heuresMax: Int): Unit = {
      var lst = List[BoolVar]()
      
      var cc = 0
      
      for (i <- List.range(0, jhl.length)) {
        val state = BoolVar("cours-prof")
        val p = getProfesseur(prof)
        val c = getCours(cours)
        
        println(p + " - " + c + " - " + heuresMax + " - " + jhl.length + " - " + jhl_prof.length + " - " + cc)
        cc += 1
        
        state <=> AND(jhl_prof(i) #= getProfesseur(prof), jhl_cours(i) #= getCours(cours))
        lst ::= state
      }
      
      sum(lst) #= heuresMax
    }
    
    /*
     * // TODO -- ALGO DE ALEX
     /*********************************************************
	 * Contraintes générales nécessitant une somme
	 * - Max d'heures pour tel cours
	 * - Max d'heures pour tel prof
	 *
	 *
	 *********************************************************/
	 var ListeC1S1 = List[BoolVar]()
	 var ListeC1S2 = List[BoolVar]()
	 for(s <- slots){
	 val statement = BoolVar("serie1")
	 statement <=> AND(s._2 #= 1, s._3 #=1)
	 ListeC1S1 ::= statement
	 
	 val statement2 = BoolVar("serie2")
	 statement2 <=> AND(s._2 #= 1, s._3 #=2)
	 ListeC1S2 ::= statement2
	 }
	
	 sum(ListeC1S1) #= 1
	 sum(ListeC1S2) #= 1
	 
     * */
    
    // ******************
    
    //TODO réfléchir à comment résoudre le problème de pas de prof à une certaine heure (par exemple pas cours le mardi dernière heure)
//    profDonnePasCoursJour("Donatien", "Vendredi")
//    profDonnePasCoursJour("Brigitte", "Lundi")
//    profDonnePasCoursJour("Brigitte", "Mardi")
//    profDonnePasCoursJour("Brigitte", "Mercredi")
//    profDonnePasCoursJour("Brigitte", "Jeudi")
    profDonneCours("Donatien", "IOO", 4)
    profDonneCours("Donatien", "Scala", 5)
//    profDonneCours("Brigitte", "Scala", 8)
    
    val vars = jhl_prof ::: jhl_cours
    
    // compteur de solutions
    var compteur = 0;
    
    def printSol(): Unit = {
      var result = Map[String, (String, String)]()
      
      // création des différentes entrées
      for (v <- jhl) {
        result += (v -> ("", ""))
      }
      
      // remplissage des entrées
      for(v <- vars) {
        val id = jhl(v.id.replaceAll("jhl_prof_", "").replaceAll("jhl_cours_", "").toInt)
        
        if (v.id.startsWith("jhl_prof_")) {
          result += (id -> (prof(v.value() - 1), result(id)._2))
        } else if (v.id.startsWith("jhl_cours_")) {
          result += (id -> (result(id)._1, cours(v.value() - 1)))
        }
      }
      
      // affichage des entrées dans l'ordre du lundi 8h30 au 017 jusqu'au vendredi 16h00 au 019
      for (v <- jhl) {
        println(v + " - " + result.get(v).get._1 + " - " + result.get(v).get._2)
      }
      
      compteur += 1
      
      println()
    }
    
    println("TROOOOOllll")
    
    //REMARQUE: faire satisfyAll si l'on veut toutes les possibilités d'horaires
    val result = satisfy(search(vars, first_fail, indomain_min), printSol)
    //val result = satisfyAll(search(vars, first_fail, indomain_min), printSol)
    
    if (!result)
      println("!!! PAS DE SOLUTION !!!")
    else
      println("!!! FIN (avec " + compteur + " SOLUTION(S)) !!!")
  }
}
