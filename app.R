library(shinydashboard)
library(shiny)
library(bslib)
library(reticulate)
source('connexion.R')
library(reticulate)
library(ggraph)
source("BD.R")
library(neo4r)
library(dplyr)
library(tidygraph)
library(ggraph)
library(DT)
#connexion()
#use_virtualenv("neo4j_env", required = TRUE)

Var<-c("ElementsExamenClinique","ElementsExamenParaclinique","Diagnostique","Traitement","DecouvertePresOperatoires","suiviPostOperatoireOuComplication")
ui<-dashboardPage(
  dashboardHeader(title = "Bienvenu Dr Boob"),
  dashboardSidebar("MENU ",
                   sidebarMenu(
                     menuItem("Enregistrement",tabName = "Enregistrement",icon = icon("user-plus")),
                     menuItem("Consultation",tabName = "Consultation",icon = icon("stethoscope")),
                     menuItem("Affichage",tabName = "Analyse",icon = icon("eye"),startExpanded = F,
                              menuSubItem("Affichage des patients",tabName = "Affichage"),
                              menuSubItem("Affichage des consultations",tabName = "consultation")
                              ),
                     menuItem("Gestion",tabName = "Supression",icon = icon("trash")),
                     selectInput("axe","Choisir un axe",choices = c("Jour","Mois","Annee")),
                     dateInput("date","Entrer une datte")
                   )
                   ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Arrière-plan général */
      body, .content-wrapper {
        background-color: #dbe4e9 !important;
      }

      /* Titre principal */
      .main-header .logo {
        background-color: #3a4a5a !important;
        color: #ffffff !important;
      }

      /* Barre de navigation */
      .main-header .navbar {
        background-color: #4a5a6a !important;
      }

      /* Sidebar */
      .main-sidebar {
        background-color: #3a4a5a !important;
      }

      /* Menu dans la sidebar */
      .sidebar-menu > li.active > a {
        background-color: #5f7d95 !important;
      }

      /* Box */
      .box {
        background-color: #e3e9ef !important;
        border-top: 3px solid #5f7d95;
        border-radius: 10px;
        box-shadow: 2px 2px 8px rgba(1,1,1,0.1);
      }

      .box-header {
        background-color: #cfdbe4 !important;
        color: #34495e !important;
        font-weight: bold;
        border-bottom: 1px solid #bccad3;
      }
    "))
    ),
    tabItems(
      tabItem(tabName = "Enregistrement",
              fluidRow(box("Information sur le patient",width=12,textInput("nom","Nom du patient"),textInput("prenom",'Prenom du patient'),numericInput("age","Age du patient",value = 0),selectInput('sexe',"Sexe",choices = c("M","F")) ),
                           box( "Du statut du Patient",width = 12,textInput("region","Entrer votre région d'origine "),textInput("quartier","Quartier"),textInput("numero","Numero"),textInput("profession","Profession"),textInput("etude","Niveau d'etude") )) ,
                       
              actionButton("confirm1","Enregistrement",icon = icon("check"), class = "btn-success")
              ),
      tabItem(
        tabName = "Consultation",box(width=12,title = "Séléctionner un élément de consultation",selectInput("consult","Element de consultation",choices =Var ),textInput("patient","Selectionner un patient")),
        
        uiOutput("ZoneDeSaisie"),fileInput('imageExamen',"Téléverser une image",accept = c("image/png","image/jpeg"))
        
      ),
      tabItem(tabName = "AnalyseGlobale",fluidRow(box("Image1",width = 4,plotOutput('fig1')),
                                           box("Image2",width=4,plotOutput("fig2")),
                                           box("Image3",width=4,plotOutput("fig3"))
                                           ),
              fluidRow(box("Image4",width=3,plotOutput("h4")),
                       box("Image5",width=3,plotOutput("h5")),
                       box("Image6",width=3,plotOutput("h6")),
                       box("Image7",width=3,plotOutput("h7"))
                       )
              ),
      tabItem(tabName = "Affichage",DTOutput("lesPatients")),
      tabItem(tabName = "consultation",DTOutput("consult")),
      tabItem(tabName = "Supression",
              fluidRow(box(width=12,
                           textInput("numASuprimer","Entrer le numéreau du patient à suprimer"),
                           actionButton("suprimer","Suprimer") )
                       
              )
    )
  )
))

server<-function(input,output){
  
  observeEvent(input$confirm1,{
    nom<-input$nom;
    prenom<-input$prenom;
    age<-input$age;
    sexe<-input$sexe;
    region<-input$region;
    quartier<-input$quartier;
    numero<-input$numero
    profession<-input$profession;
    etude<-input$etude;
    champs <- list(nom, prenom, age, sexe, region, quartier, numero, profession, etude)
    if(all(sapply(champs, function(x) !is.null(x) && x != "" && !is.na(x)))){
      enregistrerPatient(nom,prenom, age, sexe, region, quartier, numero, profession, etude)
      showModal(
        modalDialog("Succés","Votre enregistrement a été pris en compte",easyClose = T, footer = modalButton('Continuer'))
      )
    }
    else{
      showModal(
        modalDialog("Erreur","Il se pourait que vous ayez oublié un champ",easyClose = T,footer=modalButton("Reprendre"))
      )
    }
  });
  output$ZoneDeSaisie<-renderUI({
    ZoneSaisie(input$consult,input$patient)
  });
  observeEvent(input$confirm2,{
    withProgress(message = "Enregistrement en cours ...",value = 0.5,{
      if(relationPatientEvenement(input$patient,input$dateConsultation,input$consult,input$operations)){
        showModal(modalDialog('Succes',"Votre enregistrement a bien été pris en compte",easyClose = T,footer = modalButton("Continuer")))
      }
      else{
        showModal(modalDialog('Erreur',"Une erreur s'est produite",easyClose = T,footer = modalButton("Reprendre")))
      }
    })
  });
  observeEvent(input$confirm3,{
    enregistrerElementConsultation(input$consult,input$caracteristique);
    creerRelation(input$patient,input$consult,input$dateConsultation)
  });
  observeEvent(input$imageExamen,{
    req(input$imageExamen)
    if(!dir.exists("www/examens")){
      dir.create("www/examens",recursive=T)
    }
    nomFichier<-paste0("www/examens/",input$patient,'_',Sys.Date(),"_",input$imageExamen$name)
    file.copy(input$imageExamen$datapath,nomFichier)
    showModal(modalDialog("Succés","Image enregistrée"))
  })
  output$lesPatients <- renderDT({
    datatable(lesPatients(), options = list(pageLength = 10))
  })
  output$consult<-renderDT({
    datatable(lesConsultations())
  })
  observeEvent(input$suprimer,{
    removeModal()
    tryCatch({
      Suprimer(input$numASuprimer)
      showModal(
        modalDialog(title = "Succés","Votre supression a bien été prise en compte",footer = modalButton("OK"),easyClose = T)
      )
    },error=function(e){
      print(e$message)
      showModal(
        modalDialog(title = "Erreur","Votre supression n'a pas été prise en compte",footer = modalButton("OK"),easyClose = T)
      )
    })
  })
  
}














shinyApp(ui,server)