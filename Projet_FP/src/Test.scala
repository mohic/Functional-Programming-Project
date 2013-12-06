import JaCoP.scala._

object Test extends jacop {
  def main(args: Array[String]) {
    
    // liste des jours de la semaine
    val jours  = List("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")
    val heures = List("8h30", "9h30", "10h30", "11h30", "13h00", "14h00", "15h00", "16h00")
    val locaux = List("017", "019")
    
    // g�n�ration de la grille horaire
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
     * Obtenir l'index d'un jour de la semaine � une heure particuli�re et dans un local bien pr�cis
     * @params jour Le jour de la semaine. "Lundi", "Mardi", ...
     * @params heure L'heure de la semaine. "8h30", "9h30", ...
     * @params local Le local. "017", "019", ...
     * @return L'index du jour dans jhl. -1 si pas trouv�
     * */
    def getJourHeureLocal(jour: String, heure: String, local: String): Int = {
      val s_jhl = jour + '_' + heure + '_' + local
      
      jhl.indexOf(s_jhl)
    }
    
    /**
     * Obtenir l'index d'un professeur
     * @params professeur Le nom du professeur.
     * @return L'index du professeur. -1 si pas trouv�
     * */
    def getProfesseur(professeur: String): Int = {
      prof.indexOf(professeur)
    }
    
    /**
     * Obtenir l'index d'un cours
     * @params nomCours Le nom du cours.
     * @return L'index du cours. -1 si pas trouv�
     * */
    def getCours(nomCours: String): Int = {
      cours.indexOf(nomCours)
    }
    
    /**
     * D�finit une contrainte disant que le prof ne donne pas cours tel jour de la semaine
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
    
    //TODO r�fl�chir � comment r�soudre le probl�me de pas de prof � une certaine heure (par exemple pas cours le mardi derni�re heure
    //profDonnePasCoursJour("Donatien", "Lundi")
    //profDonnePasCoursJour("Brigitte", "Lundi")
    
//    profs(getProfesseur("Gilles")) #= courss(getCours("BI"))
    //(jhl_prof(0) == getProfesseur("Donatien")) -> (jhl_cours(0) #= getCours("Scala"))
    
//    profs(4) #= courss(3) // BI -> Gilles
    
//    profs(1) #= courss(1)
//    profs(0) #\= courss(1)
    
    val vars = jhl_prof ::: jhl_cours
    
    // compteur de solutions
    var compteur = 0;
    
    def printSol(): Unit = {
      var result = Map[String, (String, String)]()
      
      // cr�ation des diff�rentes entr�es
      for (v <- jhl) {
        result += (v -> ("", ""))
      }
      
      // remplissage des entr�es
      for(v <- vars) {
        val id = jhl(v.id.replaceAll("jhl_prof_", "").replaceAll("jhl_cours_", "").toInt)
        
        if (v.id.startsWith("jhl_prof_")) {
          result += (id -> (prof(v.value() - 1), result(id)._2))
        } else if (v.id.startsWith("jhl_cours_")) {
          result += (id -> (result(id)._1, cours(v.value() - 1)))
        }
      }
      
      // affichage des entr�es dans l'ordre du lundi 8h30 au 017 jusqu'au vendredi 16h00 au 019
      for (v <- jhl) {
        println(v + " - " + result.get(v).get._1 + " - " + result.get(v).get._2)
      }
      
      compteur += 1
      
      println()
    }
    
    //REMARQUE: faire satisfyAll si l'on veut toutes les possibilit�s d'horaires
    val result = satisfy(search(vars, first_fail, indomain_min), printSol)
    //val result = satisfyAll(search(vars, first_fail, indomain_min), printSol)
    
    if (!result)
      println("!!! PAS DE SOLUTION !!!")
    else
      println("!!! FIN (avec " + compteur + " SOLUTION(S)) !!!")
  }
}
