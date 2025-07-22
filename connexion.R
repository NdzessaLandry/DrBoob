library(neo4r)
library(dplyr)
library(tidygraph)
library(ggraph)
source("BD.R")
library(reticulate)
use_virtualenv("neo4j_env", required = TRUE)
neo4j <- import("neo4j")
Bd()
driver<-neo4j$GraphDatabase$driver(
  "neo4j://localhost:7687",
  auth=neo4j$basic_auth(Sys.getenv("NEO4J_USER"),Sys.getenv("NEO4J_PASSWORD"))
)

requetCypher<-function(query,params=NULL){
  session<-driver$session()
  result<-session$run(query,params)
  records<-result$data()
  session$close()
  return(records)
}

enregistrerPatient<-function(nom,prenom, age, sexe, region, quartier, numero, profession, etude){
  query<-"CREATE (:Patient {Nom: $nom, Age: $age, Sexe: $sexe, Region :$region, Quartier: $quartier,Numero: $numero,Profession: $profession, Etude: $etude})"
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
  ui<-box(title = paste(elt,"Pour le patient",idPatient), textInput("operations","Entrer les informations séparées d'une virgule "), dateInput("dateConsultation","Selectionner la date "),actionButton("confirm2","OK"))
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
    a<-requetCypher(query, params = params)
    return(T)
  }, error=function(e){
    return(F)
  })
}
