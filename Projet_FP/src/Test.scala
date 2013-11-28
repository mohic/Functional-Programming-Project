import JaCoP.scala._

object Test extends jacop {
  def main(args: Array[String]) {
    
    // liste des jours de la semaine
    val jours = List("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    
    // grille horaire
    val jhl = List("Lundi_08h30_017", "Lundi_08h30_019",
    			   "Lundi_09h30_017", "Lundi_09h30_019",
    			   "Lundi_10h30_017", "Lundi_10h30_019",
    			   "Lundi_11h30_017", "Lundi_11h30_019",
    			   "Lundi_12h30_017", "Lundi_12h30_019",
    			   
    			   "Mardi_08h30_017", "Mardi_08h30_019",
    			   "Mardi_09h30_017", "Mardi_09h30_019",
    			   "Mardi_10h30_017", "Mardi_10h30_019",
    			   "Mardi_11h30_017", "Mardi_11h30_019",
    			   "Mardi_12h30_017", "Mardi_12h30_019")
    
    // liste des profs
    val prof  = List("Donatien", "Brigitte", "Bernard", "Emmeline", "Gilles")
    
    // liste des cours
    val cours = List("Algorithme", "Scala", "IOO", "BI", "JSP")
    
    // listes de IntVar pour indiquer des contraintes
    val jhls   = for (i <- List.range(0, jhl.length)) yield IntVar(jhl(i), 1, jhl.length)
    val profs  = for (i <- List.range(0, prof.length)) yield IntVar(prof(i), 1, prof.length)
    val courss = for (i <- List.range(0, cours.length)) yield IntVar(cours(i), 1, cours.length)
    
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
    def profDonnepasCoursJour(prof: String, jour: String): Unit = {
      val j = jours.indexOf(jour)
      
      for(i <- 0 to 9) {
        jhls(j * 10 + i) #\= profs(getProfesseur(prof))
      }
    }
    
    // ******************
    
    alldifferent(jhls)
    alldifferent(profs)
    alldifferent(courss)
    
    profDonnepasCoursJour("Donatien", "Lundi")
    
//    profs(4) #= courss(3) // BI -> Gilles
//    profs(0) #= courss(1)
//    profs(0) #= courss(2)
//    profs(1) #= courss(0)
//    profs(3) #= courss(2)
    
//    profs(1) #= courss(1)
//    profs(0) #\= courss(1)
    
    val vars = jhls ::: profs ::: courss
    
    def printSol(): Unit = {
      var result = Map[Int, (String, String, String)]();
      
      for (v <- jhls) {
        result += (v.value -> (v.id, "", ""))
      }
      for (v <- profs) {
        result += (v.value -> (result(v.value)._1, v.id, ""))
      }
      
      for (v <- courss) {
        result += (v.value -> (result(v.value)._1, result(v.value)._2, v.id))
      }

      for (v <- result) {
        println(v._2._1 + " " + v._2._2 + " " + v._2._3)
      }
      
      println()
    }
    
    //REMARQUE: faire satisfyAll si l'on veut toutes les possibilités d'horaires
    val result = satisfy(search(vars, first_fail, indomain_min), printSol)
    
    if (!result)
      println("PAS DE SOLUTION !!!")
  }
}
