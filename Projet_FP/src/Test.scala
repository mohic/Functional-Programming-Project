import JaCoP.scala._

object Test extends jacop {
  def main(args: Array[String]) {
    
    // liste des jours de la semaine
    val jours  = List("Lundi", "Mardi")//, "Mercredi", "Jeudi", "Vendredi")
    val heures = List("8h30", "9h30")//, "10h30", "11h30", "13h00", "14h00", "15h00", "16h00")
    val locaux = List("017")//, "019")
    
    // génération de la grille horaire
    val jhl = for (j <- List.range(0, jours.length);
    			   h <- List.range(0, heures.length);
    			   l <- List.range(0, locaux.length))
    	yield jours(j) + '_' + heures(h) + '_' + locaux(l)
    
    // liste des profs
    val prof  = List("Donatien", "Brigitte")//, "Bernard", "Emmeline", "Gilles")
    
    // liste des cours
    val cours = List("Algorithme", "Scala")//, "IOO", "BI", "JSP")
    
    // listes de IntVar pour indiquer des contraintes
    val jhl_prof  = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_prof_" + i, 1, prof.length)
    val jhl_cours = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_cours_" + i, 1, cours.length)
    
    // ******** functions
    
    /**
     * Obtenir l'index d'un jour de la semaine à une heure particulière et dans un local bien précis
     * @params jour Le jour de la semaine. "Lundi", "Mardi", ...
     * @params heure L'heure de la semaine. "8h30", "9h30", ...
     * @params local Le local. "017", "019", ...
     * @return L'index du jour dans jhl. -1 si pas trouvé
     * */
    def getJourHeureLocal(jour: String, heure: String, local: String): Int = {
      val s_jhl = jour + '_' + heure + '_' + local
      
      jhl.indexOf(s_jhl)
    }
    
    /**
     * Obtenir l'index d'un professeur
     * @params professeur Le nom du professeur.
     * @return L'index du professeur. -1 si pas trouvé
     * */
    def getProfesseur(professeur: String): Int = {
      prof.indexOf(professeur)
    }
    
    /**
     * Obtenir l'index d'un cours
     * @params nomCours Le nom du cours.
     * @return L'index du cours. -1 si pas trouvé
     * */
    def getCours(nomCours: String): Int = {
      cours.indexOf(nomCours)
    }
    
    /**
     * Définit une contrainte disant que le prof ne donne pas cours tel jour de la semaine
     * @params prof Le nom du professeur
     * @params jour Le jour de la semaine
     * */
    def profDonnePasCoursJour(prof: String, jour: String): Unit = {
      val j = jours.indexOf(jour)
      
      for (i <- 0 to (heures.length * locaux.length - 1)) {
        jhl_prof(i + (j * heures.length * locaux.length)) #\= getProfesseur(prof) + 1
      }
    }
    
    // ******************
    
    profDonnePasCoursJour("Donatien", "Lundi")
    //profDonnePasCoursJour("Brigitte", "Lundi")
    
//    profs(getProfesseur("Gilles")) #= courss(getCours("BI"))
    
//    profs(4) #= courss(3) // BI -> Gilles
    
//    profs(1) #= courss(1)
//    profs(0) #\= courss(1)
    
    val vars = jhl_prof ::: jhl_cours
    
    def printSol(): Unit = {      
      for(v <- vars) {
        print(jhl(v.id.replaceAll("jhl_prof_", "").replaceAll("jhl_cours_", "").toInt) + " = ")
        
        if (v.id.startsWith("jhl_prof_")) {
          println(prof(v.value() - 1))
        } else if (v.id.startsWith("jhl_cours_")) {
          println(cours(v.value() - 1))
        } else {
          println();
        }
      }
      
      println()
    }
    
    //REMARQUE: faire satisfyAll si l'on veut toutes les possibilités d'horaires
    //val result = satisfy(search(vars, first_fail, indomain_min), printSol)
    val result = satisfyAll(search(vars, first_fail, indomain_min), printSol)
    
    if (!result)
      println("!!! PAS DE SOLUTION !!!")
    else
      println("!!! FIN !!!")
  }
}
