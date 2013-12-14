import JaCoP.scala._
import JaCoP.constraints.Sum

object Test extends jacop {
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
    val prof  = List("AUCUN", "Donatien", "Thierry", "Gilles", "Sonia")
    
    // liste des cours
    val cours = List("AUCUN", "Anglais", "Scala", "C++", "BI", "SAP")
    
    // liste des séries
    val series = List("AUCUN", "A", "B")
    
    // listes de IntVar pour indiquer des contraintes
    val jhl_prof  = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_prof_" + i, 1, prof.length)
    val jhl_cours = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_cours_" + i, 1, cours.length)
    val jhl_serie = for (i <- List.range(0, jhl.length)) yield IntVar("jhl_serie_" + i, 1, series.length)
    
    // ******** functions
    
    /**
     * Obtenir l'index d'un jour
     * @params jour Le nom du jour
     * @return L'index du jour à partir de 1. 0 si pas trouvé
     * */
    def getJour(jour: String): Int = {
      jours.indexOf(jour) + 1
    }
    
    /**
     * Obtenir l'index d'une heure
     * @params heure L'heure
     * @return L'index de l'heure à partir de 1. 0 si pas trouvé
     * */
    def getHeure(heure: String): Int = {
      heures.indexOf(heure) + 1
    }
    
    /**
     * Obtenir l'index d'un local
     * @params local Le nom du local
     * @return L'index du local à partir de 1. 0 si pas trouvé
     * */
    def getLocal(local: String): Int = {
      locaux.indexOf(local) + 1
    }
    
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
     * Obtenir l'index d'une série
     * @params nomCours Le nom de la série
     * @return L'index d'une série à partir de 1. 0 si pas trouvé
     * */
    def getSerie(nomSerie: String): Int = {
      series.indexOf(nomSerie) + 1
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
     * Définit une contrainte disant qu'un prof ne donne pas cours avant tel heure
     * @params prof Le nom du professeur
     * @params heure L'heure
     * */
    def profDonnePasCoursAvantOuApresHeure(prof: String, heure: String, arrivee: Boolean): Unit = {
      val indexHeure = getHeure(heure) - 1
      
      for (j <- jours) {
        for (l <- locaux) {
          if(arrivee) {
        	  for (h <- 0 to indexHeure) {
        		  jhl_prof(getJourHeureLocal(j, heures(h), l)) #\= (getProfesseur(prof))
        	  }
          } else {
            for (h <- indexHeure to heures.length-1) {
              //TODO: marche pas et je ne vois pas pourquoi
        		//  jhl_prof(getJourHeureLocal(j, heures(h), l)) #\= (getProfesseur(prof))
            }
          }
        }
      }
    }
    
    /**
     * Définit une contrainte disant que le professeur donne un certain cours durant autant d'heures
     * @params prof Le nom du professeur
     * @params cours Le nom du cours
     * @params nbrHeures Le nombre d'heures
     * */
    def profDonneCours(prof: String, cours: String, nbrHeures: Int): Unit = {
      var lst = List[BoolVar]()
      
      for (i <- List.range(0, jhl.length)) {
        val state = BoolVar("cours-prof")
        
        state <=> AND(jhl_prof(i) #= getProfesseur(prof), jhl_cours(i) #= getCours(cours))
        lst ::= state
      }
      
      sum(lst) #= nbrHeures
    }
    
    /**
     * Définit une contrainte disant que la série à un certain cours durant autant d'heures
     * @params serie Le nom de la série
     * @params cours Le nom du cours
     * @params nbrHeures Le nombre d'heures
     * */
    def serieCours(serie: String, cours: String, nbrHeures: Int): Unit = {
      var lst = List[BoolVar]()
      
      for (i <- List.range(0, jhl.length)) {
        val state = BoolVar("cours-serie")
        
        state <=> AND(jhl_serie(i) #= getSerie(serie), jhl_cours(i) #= getCours(cours))
        lst ::= state
      }
      
      sum(lst) #= nbrHeures
    }
    
    // contraintes obligatoires
    
    // interdire d'avoir cours si pas de prof et inversément et idem avec les séries
    for (i <- List.range(0, jhl.length))
      OR(AND(jhl_prof(i) #= 1, jhl_cours(i) #= 1, jhl_serie(i) #= 1), AND(jhl_prof(i) #\= 1, jhl_cours(i) #\= 1, jhl_serie(i) #\= 1))
      
    // un prof ne peut pas être dans plusieurs local en même temps
    for (i <- List.range(0, jhl.length, locaux.length)) {
      for (j <- 1 to locaux.length - 1) {
        OR(AND(jhl_prof(i) #= 1, jhl_prof(i) #= 1), AND(jhl_prof(i) #\= 1, jhl_prof(i) #\= jhl_prof(i + j)))
      }
    }
    
    // ******************
    
    // jour d'absence des profs
    profDonnePasCoursJour("Donatien", "Mercredi")
//    
//    profDonnePasCoursJour("Brigitte", "Jeudi")
//    
//    profDonnePasCoursJour("Gilles", "Lundi")
//    profDonnePasCoursJour("Gilles", "Mercredi")
//    profDonnePasCoursJour("Gilles", "Jeudi")
//    profDonnePasCoursJour("Gilles", "Vendredi")
//    
//    profDonnePasCoursJour("Emmeline", "Vendredi")
    
    // heure d'arrivée des profs
    profDonnePasCoursAvantOuApresHeure("Donatien", "16h00", true)
//    profDonnePasCoursAvantHeure("Brigitte", "13h00")
//    profDonnePasCoursAvantHeure("Gilles", "9h30")
    
    // heure de départ des profs
    profDonnePasCoursAvantOuApresHeure("Donatien", "15h00", false)
    
    // série a cours de ... pendant ... heures
//    serieCours("A", "Anglais", 2)
//    serieCours("B", "Anglais", 2)
//    
//    serieCours("A", "BI", 2)
//    serieCours("B", "BI", 2)
//    
//    serieCours("A", "C++", 2)
    serieCours("A", "Scala", 2)
    
//    serieCours("B", "SAP", 2)
    serieCours("B", "Scala", 2)
    
    // prof donne cours pendant ... heures
//    profDonneCours("Sonia", "Anglais", 4)
//    profDonneCours("Gilles", "BI", 4)
//    profDonneCours("Thierry", "C++", 2)
    profDonneCours("Donatien", "Scala", 4)
//    profDonneCours("Thierry", "SAP", 2)
    
    
    //TODO: profDonnePasCoursApres... profDonnePasCoursATelHeure...
    
    
    
//    profDonnePasCoursJour("Donatien", "Vendredi")
//    profDonnePasCoursJour("Brigitte", "Lundi")
//    profDonnePasCoursJour("Brigitte", "Mardi")
//    profDonnePasCoursJour("Brigitte", "Mercredi")
//    profDonnePasCoursJour("Brigitte", "Jeudi")
    //profDonnePasCoursAvantHeure("Donatien", "9h30")
//    profDonneCours("Donatien", "IOO", 4)
    //profDonneCours("Donatien", "Scala", 4)
//    serieACours("A", "IOO", 3)
//    serieACours("B", "IOO", 3)
//    profDonneCours("Brigitte", "IOO", 2)
//    profDonneCours("Gilles", "BI", 4)
    
    val vars = jhl_prof ::: jhl_cours ::: jhl_serie
    
    // compteur de solutions
    //var compteur = 0;
    
    def printSol(): Unit = {
      var result = Map[String, (String, String, String)]()
      
      // création des différentes entrées
      for (v <- jhl) {
        result += (v -> ("", "", ""))
      }
      
      // remplissage des entrées
      for(v <- vars) {
        val id = jhl(v.id.replaceAll("jhl_prof_", "").replaceAll("jhl_cours_", "").replaceAll("jhl_serie_", "").toInt)
        
        if (v.id.startsWith("jhl_prof_")) {
          if (v.value() - 1 == 0)
            result += (id -> ("°", result(id)._2, result(id)._3))
          else
        	  result += (id -> (prof(v.value() - 1), result(id)._2, result(id)._3))
        } else if (v.id.startsWith("jhl_cours_")) {
          if (v.value() - 1 == 0)
            result += (id -> (result(id)._1, "°", result(id)._3))
          else
        	  result += (id -> (result(id)._1, cours(v.value() - 1), result(id)._3))
        } else if (v.id.startsWith("jhl_serie_")) {
          if (v.value() - 1 == 0)
            result += (id -> (result(id)._1, result(id)._2, "°"))
          else
        	  result += (id -> (result(id)._1, result(id)._2, series(v.value() - 1)))
        }
      }
      
      // affichage des entrées dans l'ordre du lundi 8h30 au 017 jusqu'au vendredi 16h00 au 019
      for (v <- jhl) {
        println(v + " - " + result.get(v).get._1 + " - " + result.get(v).get._2 + " - " + result.get(v).get._3)
      }
      
      //compteur += 1
      
      println()
    }
    
    //REMARQUE: faire satisfyAll si l'on veut toutes les possibilités d'horaires
    val result = satisfy(search(vars, first_fail, indomain_min), printSol)
    //val result = satisfyAll(search(vars, first_fail, indomain_min), printSol)
    
    if (!result)
      println("!!! PAS DE SOLUTION !!!")
    else
      println("!!! FIN") // (avec " + compteur + " SOLUTION(S)) !!!")
  }
}
