library(neo4r)
library(dplyr)
library(tidygraph)
library(ggraph)
source("BD.R")
library(reticulate)
use_python("C:/Users/DELL/Documents/.virtualenvs/neo4j_env/Scripts/python.exe", required = TRUE)
connexion1<-function(){
  use_python("C:\\Users\\DELL\\Documents\\.virtualenvs\\neo4j_env\\Scripts\\python.exe", required = TRUE)
  use_virtualenv("C:/Users/DELL/Documents/.virtualenvs/neo4j_env", required = TRUE)
  neo4j <- import("neo4j")
  #Bd()
  Sys.setenv(UV_OFFLINE=1)
  py_require()
  driver<<-neo4j$GraphDatabase$driver(
    "neo4j://localhost:7687",
    auth=neo4j$basic_auth("neo4j","landry2003")
  )
}
connexion <- function() {
  #library(reticulate)
  
  # Activer l'environnement virtuel Python
  #use_virtualenv("C:/Users/DELL/Documents/.virtualenvs/neo4j_env", required = TRUE)
  
  # Importer le driver Neo4j
  neo4j <- import("neo4j")
  
  # Connexion au serveur Neo4j
  driver <<- neo4j$GraphDatabase$driver(
    "neo4j://localhost:7687",
    auth = neo4j$basic_auth("neo4j", "landry2003")
  )
}



requetCypher<-function(query,params=NULL){
  session<-driver$session()
  result<-session$run(query,params)
  records<-result$data()
  session$close()
  return(records)
}

enregistrerPatient<-function(nom,prenom, age, sexe, region, quartier, numero, profession, etude){
  query<-"CREATE (:Patient {Nom: $nom, Prenom:$prenom,Age: $age, Sexe: $sexe, Region :$region, Quartier: $quartier,Numero: $numero,Profession: $profession, Etude: $etude})"
  params<-dict(
    nom=nom,
    prenom=prenom,
    age=age,
    sexe=sexe,
    region=region,
    quartier=quartier,
    numero=numero,
    profession=profession,
    etude=etude
  )
  requetCypher(query,params=params)
}

ZoneSaisie<-function(elt,idPatient){
  ui<-box(width=12,title = paste(elt,"Pour le patient",idPatient), textInput("operations","Entrer les informations séparées d'une virgule "), dateInput("dateConsultation","Selectionner la date "),actionButton("confirm2","OK",icon = icon("check"), class = "btn-success"))
return(ui)
}

relationPatientEvenement<-function(patient,date,consul,listeEvenement){
  elt<-strsplit(listeEvenement,split=",")[[1]]
  #relation<-paste0("relation","_",consul)
  elt <- trimws(elt)
  print("Éléments (elt) :")
  print(elt)
  params <- dict(
    elt = elt,
    num = patient,
    date = date
  )
  print("Paramètres envoyés :")
  print(params)
  # Construire la requête dynamiquement
  query <- sprintf("
    MATCH (p:Patient {Numero: $num})
    UNWIND $elt AS e
    MERGE (el:%s {nom:e})
    MERGE (p)-[r:%s]->(el)
    SET r.date = $date
  ", consul, consul)
  
  tryCatch({
    if(patient=="" || listeEvenement==""){
      return(F)
    }
    a<-requetCypher(query, params = params)
    return(T)
  }, error=function(e){
    return(F)
  })
}




lesPatients <- function() {
  query <- "
    MATCH (p:Patient)
    RETURN 
      p.Nom AS Nom,
      p.Prenom AS Prenom,
      p.Age AS Age,
      p.Sexe AS Sexe,
      p.Region AS Region,
      p.Quartier AS Quartier,
      p.Numero AS Numero,
      p.Profession AS Profession,
      p.Etude AS Etude
  "
  res <- requetCypher(query)
  res<-reticulate::py_to_r(res)
  res<-do.call(rbind, lapply(res, as.data.frame, stringsAsFactors = FALSE))
  if (is.null(res) || length(res) == 0) {
    return(data.frame(
      Nom = character(),
      Prenom = character(),
      Age = numeric(),
      Sexe = character(),
      Region = character(),
      Quartier = character(),
      Numero = character(),
      Profession = character(),
      Etude = character()
    ))
  }
  
  return(as.data.frame(res))
}

lesConsultations<-function(){
  query<-"MATCH (p:Patient)-[r]->(t)
  RETURN 
  p.Nom AS Nom,
  p.Prenom AS Prenom,
  p.Age AS AgeIndividu,
  p.Numero as Numero,
  toString(r.date) AS DateConsultation,
  t.nom as Observation,
  type(r) AS Consultation
"
  res <- requetCypher(query)
  res<-reticulate::py_to_r(res)
  res<-do.call(rbind, lapply(res, as.data.frame, stringsAsFactors = FALSE))
  if (is.null(res) || length(res) == 0) {
    return(data.frame(
      Nom = character(),
      Prenom = character(),
      Age = numeric(),
      Sexe = character(),
      Region = character(),
      Quartier = character(),
      Numero = character(),
      Profession = character(),
      Etude = character()
    ))
    
  }
  return(res)
}

Suprimer<-function(num){
  query <- sprintf("
    MATCH (p:Patient {Numero: '%s'})
    DETACH DELETE p
  ", num)
  
  res <- requetCypher(query)
  invisible(res) # pas besoin de retourner un résultat
}

connexion()
#Bd()







