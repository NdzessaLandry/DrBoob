library(shinydashboard)
library(shiny)
library(bslib)
source('connexion.R')
library(reticulate)
library(ggraph)
source("BD.R")
use_virtualenv("neo4j_env", required = TRUE)
Var<-c("ElementsExamenClinique","ElementsExamenParaclinique","Diagnostique","Traitement","DecouvertePresOperatoires","suiviPostOperatoireOuComplication")
ui<-dashboardPage(
  dashboardHeader(title = "Bienvenu Dr Boob"),
  dashboardSidebar("Les axes d' analyses ",
                   sidebarMenu(
                     menuItem("Enregistrement",tabName = "Enregistrement"),
                     menuItem("Consultation",tabName = "Consultation"),
                     menuItem("Analyse",tabName = "Analyse"),
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
              fluidRow(box(width=12,box("Information sur le patient",width=12,textInput("nom","Nom du patient"),textInput("prenom",'Prenom du patient'),numericInput("age","Age du patient",value = 0),selectInput('sexe',"Sexe",choices = c("M","F"))),
                           box("Du statut du Patient",width = 12,textInput("region","Entrer votre région d'origine "),textInput("quartier","Quartier"),textInput("numero","Numero"),textInput("profession","Profession"),textInput("etude","Niveau d'etude")))
                       ),
              actionButton("confirm1","Enregistrement")
              ),
      tabItem(
        tabName = "Consultation",box(width=12,title = "Séléctionner un élément de consultation",selectInput("consult","Element de consultation",choices =Var ),textInput("patient","Selectionner un patient")),
        
        uiOutput("ZoneDeSaisie"),
      ),
      tabItem(tabName = "Analyse",fluidRow(box("Image1",width = 4,plotOutput('fig1')),
                                           box("Image2",width=4,plotOutput("fig2")),
                                           box("Image3",width=4,plotOutput("fig3"))
                                           ),
              fluidRow(box("Image4",width=3,plotOutput("h4")),
                       box("Image5",width=3,plotOutput("h5")),
                       box("Image6",width=3,plotOutput("h6")),
                       box("Image7",width=3,plotOutput("h7"))
                       )
              ),
      tabItem(tabName = "Prediction")
    )
  )
)

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
    if(relationPatientEvenement(input$patient,input$dateConsultation,input$consult,input$operations)){
      showModal(modalDialog('Succes',"Votre enregistrement a bien été pris en compte",easyClose = T,footer = modalButton("Continuer")))
    }
    else{
      showModal(modalDialog('Erreur',"Une erreur s'est produite",easyClose = T,footer = modalButton("Reprendre")))
    }
  });
  observeEvent(input$confirm3,{
    enregistrerElementConsultation(input$consult,input$caracteristique);
    creerRelation(input$patient,input$consult,input$dateConsultation)
  });
  
}














shinyApp(ui,server)