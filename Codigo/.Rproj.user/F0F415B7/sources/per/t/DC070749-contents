##-libraries-##
library(shinydashboard)
library("plotly")
library("shiny")
library("dplyr")
library("ggplot2")
library("readxl")
library("sqldf")
library("data.table")

choiceDPLYR=c("DPLYR1","DPLYR2","DPLYR3","DPLYR4","DPLYR5","DPLYR6","DPLYR7","DPLYR8","DPLYR9","DPLYR10",
              "DPLYR11","DPLYR12","DPLYR13","DPLYR14","DPLYR15","DPLYR16","DPLYR17","DPLYR18","DPLYR19","DPLYR20")
choiceSQLDF=c("SQLDF1","SQLDF2","SQLDF3","SQLDF4","SQLDF5","SQLDF6","SQLDF7","SQLDF8","SQLDF9","SQLDF10",
              "SQLDF11","SQLDF12","SQLDF13","SQLDF14","SQLDF15","SQLDF16","SQLDF17","SQLDF18","SQLDF19","SQLDF20")
choiceDT=c("DT1","DT2","DT3","DT4","DT5","DT6","DT7","DT8","DT9","DT10","DT11",
           "DT12","DT13","DT14","DT15","DT16","DT17","DT18","DT19","DT20")
grafGG=c("GG1","GG2","GG3","GG4","GG5","GG6","GG7","GG8","GG9","GG10","GG11","GG12","GG13",
         "GG14","GG15","GG16","GG17","GG18","GG19","GG20")
grafPL=c("plotly1","plotly2","plotly3","plotly4","plotly5","plotly6","plotly7","plotly8","plotly9","plotly10","plotly11",
         "plotly12","plotly13","plotly14","plotly15","plotly16","plotly17","plotly18","plotly19","plotly20")
cur<-c("Introduccion a la Ingenieria de Sistemas","Introduccion a la Programacion","Lenguaje","Matematica Basica",
       "Programacion Orientada a Objetos","Automatas y Compiladores","Calculo","Matematica Discreta",
       "Fisica 1","Matematica III","Fundamentos de Sistemas de Informacion","Algebra Lineal",
       "Fisica aplicada a la computacion","Organizacion y gestion de empresas","Estructura de datos","Arquitectura de computadoras",
       "Estadistica y Probabilidades","Base de datos","Interaccion Hombre Computador","Ingenieria de Software 1",
       "Desarrollo de Aplicaciones","Ingenieria de Software 2","Modelado de Proceso de Negocios 1","Sistema Gestor de Base de Datos","Sistemas Operativos",
       "Planeamiento Estrategico de TI","Investigacion de Operaciones","Sistemas de Informacion","Modelado de Proceso de Negocios 2","Arquitectura de Redes de Computadoras",
       "Administracion de Redes y Seguridad de la Informacion","Sistema de Soporte de Decision","Proyecto de Investigacion","Administacion de Proyectos de Sistemas de Informacion","Gestion de TI")

##-ui-##
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title="Proyecto Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Recoleccion", tabName = "dashboard", icon = icon("fas fa-upload",lib="font-awesome")),
      menuItem("Pre-Procesamiento", tabName = "preprocesamiento", icon = icon("cog",lib="glyphicon")),
      menuItem("Exploracion", tabName = "exploracion", icon = icon("fas fa-search",lib="font-awesome")),
      menuItem("Graficos", tabName = "graficos", icon = icon("stats",lib="glyphicon")),
      menuItem("Modelo", tabName = "modelo", icon = icon("fas fa-trophy",lib="font-awesome"))
    )
  ),
  dashboardBody(
    tabItems(
      #First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title="Carga de Datos",status="success",solidHeader=TRUE,
                    fileInput("idArchivo","Seleccione un archivo XLSX",accept=c(".xlsx")),
                    numericInput("idSheet",label="Ingrese el número de hoja",value=1,min=1,max=4)
                ),
                box(title="Descripcion del Proyecto",status="info",solidHeader=FALSE,
                    "Nuestro proyecto analiza el Rendimiento Academico de los alumnos de la escuela de ICSI
                    de la universidad UPAO utilizando Data Mining. El objetivo principal es predecir notas mediante
                    un modelo de regresion lineal",br(),"Integrantes:",br(),"Neyra Ocana, Leonardo",
                    br(),"Ramos Saravia, Sandro",tags$hr(),"Universidad Privada Antenor Orrego - UPAO"
                )
              ),
              fluidRow(
                box(title = "Dataset Seleccionado",status="warning",width=12,solidHeader=TRUE,
                  tableOutput(outputId="plotRecoleccion")
                )
              )
      ),
      #Second tab content
      tabItem(tabName = "preprocesamiento",
              fluidRow(
                tabBox(title="Etapas",id="tabBox1",width=12,
                       tabPanel("RawData","Un dataset de un curso en crudo",br(),"Usamos el paquete readxl",tags$hr(),tableOutput("view")),
                       tabPanel("AddCol",
                                "Transformacion",br(),"Agragamos columnas indicando el nombre del curso y el ciclo",
                                br(),"Usamos DPLYR",
                                tags$hr(),tableOutput("addCol")),
                       tabPanel("CleanHead",
                                "Imputacion",br(),"Eliminamos obs. innecesarias",br(),"Usamos DPLYR",tags$hr(),
                                "Transformacion",br(),"Agregamos nombre a las cabeceras",br(),"Usamos el paquete base",
                                tags$hr(),tableOutput("cleanHead")),
                       tabPanel("Normalization",
                                "Se normalizo las columnas por medio de la siguiente forma",br(),
                                verbatimTextOutput(outputId="TextNorm"),br(),
                                tableOutput("NormView")),
                       tabPanel("FinalDataset",
                                "Transformacion",br(),"Concatenamos todos los datasets de cursos en un solo dataset llamado 'ICSI'",
                                br(),"Convertimos algunas columnas de Character a Numeric",br(),"Usamos Janitor",tags$hr(),
                                "Imputacion",br(),"Eliminamos a los alumnos inhabilitados, o sea las obs. que incluyen 'IN'",
                                br(),"Usamos DPLYR",
                                tags$hr(),tableOutput("finalDataset")))
              )
      ),
      #Third tab content
      tabItem(tabName = "exploracion",
              fluidRow(
                tabBox(title="Consultas de Exploracion",id="tabBox2",width=12,
                       tabPanel("Consultas con DPLYR",
                                selectInput("consultasDPLYR","Elige una consulta DPLYR",choices=choiceDPLYR),
                                textOutput("textoConsultasDPLYR"),
                                tags$hr(),
                                tableOutput("consultasViewDPLYR")),
                       tabPanel("Consultas con SQLDF",
                                selectInput("consultasSQLDF","Elige una consulta SQLDF",choices=choiceSQLDF),
                                textOutput("textoConsultasSQLDF"),
                                tags$hr(),
                                tableOutput("consultasViewSQLDF")),
                       tabPanel("Consultas con data.table",
                                selectInput("consultasDT","Elige una consulta SQLDF",choices=choiceDT),
                                textOutput("textoConsultasDT"),
                                tags$hr(),
                                tableOutput("consultasViewDT"))
                        )
              )
      ),
      #Fourth tab content
      tabItem(tabName = "graficos",
              fluidRow(
                tabBox(title="Graficos de Exploracion",width=12,
                       tabPanel("Graficos con GGPLOT",
                                selectInput("graficosGG","Elige un grafico ggplot",choices=grafGG),
                                textOutput("textoGraficosGG"),
                                tags$hr(),
                                plotOutput("graficosViewGG")),
                       tabPanel("Graficos con PLOTLY",
                                selectInput("graficosPL","Elige un grafico plotly",choices=grafPL),
                                textOutput("textoGraficosPL"),
                                tags$hr(),
                                plotlyOutput("graficosViewPL"))
                       )
              )
      ),
      #Fifth tab content
      tabItem(tabName = "modelo",
              fluidRow(
                box(title="Modelo de Regresion Lineal",status="info",solidHeader=TRUE,
                    selectInput("curso1","Elige el primer curso",choices=cur),
                    selectInput("curso2","Elige el segundo curso",choices=cur),
                    numericInput("notaX",label="Ingrese nota a predecir (X value)",value=14,min=1,max=20)
                    ),
                box(title="Datos del Modelo",status="info",solidHeader=TRUE,
                    h5("Coeficiente de Pearson"),
                    textOutput("textoPearson"),
                    tags$hr(),
                    h5("Nuevo valor de nota (Y value)"),
                    textOutput("textoNewY"))
              ),
              fluidRow(
                box(title="Grafico del Modelo de Regresion Lineal",status="warning",width=12,solidHeader=TRUE,
                    plotOutput("modeloView")
                )
              )
      )
    )
  )
)

##-server-##
server <- function(input, output) {
  #Logica del panel "Recoleccion"
  output$plotRecoleccion<-renderTable({
    req(input$idArchivo)
    tryCatch({
      inFile<-input$idArchivo
      dat<-read_xlsx(inFile$datapath,sheet=input$idSheet)
    }, error=function(e){stop(safeError(e))})
    return(dat)
  })
  
  #Logica del panel de "Pre-Procesamiento"
  output$view<-renderTable({raw1IntroIngSist})
  
  raw1IntroIngSistM<-mutate(raw1IntroIngSist,Curso="Introduccion a la Ingenieria de Sistemas",Ciclo=1)
  output$addCol<-renderTable({raw1IntroIngSistM})
  
  raw1IntroIngSistMP<-ProcesarCurso(raw1IntroIngSistM)
  output$cleanHead<-renderTable({raw1IntroIngSistMP})
  
  output$TextNorm<-renderText({"normal<-function(x){(x-min(x))/(max(x)-min(x))}
  dtNormal<-data.frame(ICSI%>%select(Final))
  dtNormal<-normal(dtNormal$Final)
  View(dtNormal)"})
  
  output$NormView<-renderTable({head(dtNormal)})
  
  output$finalDataset<-renderTable({ICSI})
  
  #Logica del panel de "Exploracion de Datos"
  output$consultasViewDPLYR<-renderTable({
    if(input$consultasDPLYR=="DPLYR1"){DPLYR1}
    else if(input$consultasDPLYR=="DPLYR2"){DPLYR2}
    else if(input$consultasDPLYR=="DPLYR3"){DPLYR3}
    else if(input$consultasDPLYR=="DPLYR4"){DPLYR4}
    else if(input$consultasDPLYR=="DPLYR5"){DPLYR5}
    else if(input$consultasDPLYR=="DPLYR6"){DPLYR6}
    else if(input$consultasDPLYR=="DPLYR7"){DPLYR7}
    else if(input$consultasDPLYR=="DPLYR8"){DPLYR8}
    else if(input$consultasDPLYR=="DPLYR9"){DPLYR9}
    else if(input$consultasDPLYR=="DPLYR10"){DPLYR10}
    else if(input$consultasDPLYR=="DPLYR11"){DPLYR11}
    else if(input$consultasDPLYR=="DPLYR12"){DPLYR12}
    else if(input$consultasDPLYR=="DPLYR13"){DPLYR13}
    else if(input$consultasDPLYR=="DPLYR14"){DPLYR14}
    else if(input$consultasDPLYR=="DPLYR15"){DPLYR15}
    else if(input$consultasDPLYR=="DPLYR16"){DPLYR16}
    else if(input$consultasDPLYR=="DPLYR17"){DPLYR17}
    else if(input$consultasDPLYR=="DPLYR18"){DPLYR18}
    else if(input$consultasDPLYR=="DPLYR19"){DPLYR19}
    else if(input$consultasDPLYR=="DPLYR20"){DPLYR20}
  })
  output$consultasViewSQLDF<-renderTable({
    if(input$consultasSQLDF=="SQLDF1"){SQL1}
    else if(input$consultasSQLDF=="SQLDF2"){SQL2}
    else if(input$consultasSQLDF=="SQLDF3"){SQL3}
    else if(input$consultasSQLDF=="SQLDF4"){SQL4}
    else if(input$consultasSQLDF=="SQLDF5"){SQL5}
    else if(input$consultasSQLDF=="SQLDF6"){SQL6}
    else if(input$consultasSQLDF=="SQLDF7"){SQL7}
    else if(input$consultasSQLDF=="SQLDF8"){SQL8}
    else if(input$consultasSQLDF=="SQLDF9"){SQL9}
    else if(input$consultasSQLDF=="SQLDF10"){SQL10}
    else if(input$consultasSQLDF=="SQLDF11"){SQL11}
    else if(input$consultasSQLDF=="SQLDF12"){SQL12}
    else if(input$consultasSQLDF=="SQLDF13"){SQL13}
    else if(input$consultasSQLDF=="SQLDF14"){SQL14}
    else if(input$consultasSQLDF=="SQLDF15"){SQL15}
    else if(input$consultasSQLDF=="SQLDF16"){SQL16}
    else if(input$consultasSQLDF=="SQLDF17"){SQL17}
    else if(input$consultasSQLDF=="SQLDF18"){SQL18}
    else if(input$consultasSQLDF=="SQLDF19"){SQL19}
    else if(input$consultasSQLDF=="SQLDF20"){SQL20}
  })
  output$consultasViewDT<-renderTable({
    if(input$consultasDT=="DT1"){DT1}
    else if(input$consultasDT=="DT2"){DT2}
    else if(input$consultasDT=="DT3"){DT3}
    else if(input$consultasDT=="DT4"){DT4}
    else if(input$consultasDT=="DT5"){DT5}
    else if(input$consultasDT=="DT6"){DT6}
    else if(input$consultasDT=="DT7"){DT7}
    else if(input$consultasDT=="DT8"){DT8}
    else if(input$consultasDT=="DT9"){DT9}
    else if(input$consultasDT=="DT10"){DT10}
    else if(input$consultasDT=="DT11"){DT11}
    else if(input$consultasDT=="DT12"){DT12}
    else if(input$consultasDT=="DT13"){DT13}
    else if(input$consultasDT=="DT14"){DT14}
    else if(input$consultasDT=="DT15"){DT15}
    else if(input$consultasDT=="DT16"){DT16}
    else if(input$consultasDT=="DT17"){DT17}
    else if(input$consultasDT=="DT18"){DT18}
    else if(input$consultasDT=="DT19"){DT19}
    else if(input$consultasDT=="DT20"){DT20}
  })
  
  output$textoConsultasDPLYR<-renderText({
    if(input$consultasDPLYR=="DPLYR1"){"inhabilitados y habilitados en mateBasica"}
    else if(input$consultasDPLYR=="DPLYR2"){"aprobados y desaprobados en 1°ciclo"}
    else if(input$consultasDPLYR=="DPLYR3"){"aprobados de IntroProg y POO"}
    else if(input$consultasDPLYR=="DPLYR4"){"Nota media del curso de calculo"}
    else if(input$consultasDPLYR=="DPLYR5"){"Varianza en el parcial de Lenguaje"}
    else if(input$consultasDPLYR=="DPLYR6"){"Promedio de notas hasta el parcial de mate3"}
    else if(input$consultasDPLYR=="DPLYR7"){"Promedio de nota Final curso de mate3"}
    else if(input$consultasDPLYR=="DPLYR8"){"Comparación de notas de la 1 y 2 mitad de mate3"}
    else if(input$consultasDPLYR=="DPLYR9"){"Notas de alumnos del curso de fisica1"}
    else if(input$consultasDPLYR=="DPLYR10"){"alumnos que están por encima de la media del curso de EstruDatos"}
    else if(input$consultasDPLYR=="DPLYR11"){"Top 3 de alumnos del 3ciclo"}
    else if(input$consultasDPLYR=="DPLYR12"){"Desviacion Estandar del examen final de FisCom"}
    else if(input$consultasDPLYR=="DPLYR13"){"Alumno con mayor promedio de 1ciclo"}
    else if(input$consultasDPLYR=="DPLYR14"){"top10 de alumnos IntroProg"}
    else if(input$consultasDPLYR=="DPLYR15"){"top10 de alumnos POO"}
    else if(input$consultasDPLYR=="DPLYR16"){"Join entre IntroProg y POO"}
    else if(input$consultasDPLYR=="DPLYR17"){"Alumnos top5 alumnos Sisope"}
    else if(input$consultasDPLYR=="DPLYR18"){"Alumnos top5 alumnos ArquiComp"}
    else if(input$consultasDPLYR=="DPLYR19"){"Join entre Sisope y ArquiComp"}
    else if(input$consultasDPLYR=="DPLYR20"){"Cantidad entre aprobados de MPN1 y MPN2"}
  })
  output$textoConsultasSQLDF<-renderText({
    if(input$consultasSQLDF=="SQLDF1"){"aprobados y desaprobados en IntroIngSI"}
    else if(input$consultasSQLDF=="SQLDF2"){"aprobados en el curso de IntroProg"}
    else if(input$consultasSQLDF=="SQLDF3"){"Alumnos que no están invictos hasta 3 ciclo"}
    else if(input$consultasSQLDF=="SQLDF4"){"Alumnos desaprobados en IntroProg"}
    else if(input$consultasSQLDF=="SQLDF5"){"Alumnos desaprobados en MateBasica"}
    else if(input$consultasSQLDF=="SQLDF6"){"Join alumnos desaprobados en POO e IntroProg"}
    else if(input$consultasSQLDF=="SQLDF7"){"Relacion de alumnos de 1ciclo y 7ciclo"}
    else if(input$consultasSQLDF=="SQLDF8"){"Cantidad de registros de alumnos desaprobados por ciclo"}
    else if(input$consultasSQLDF=="SQLDF9"){"Cantidad de registros de alumnos por ciclo"}
    else if(input$consultasSQLDF=="SQLDF10"){"Tasa de desaprobació de ICSI según ciclo en el semestre 201510"}
    else if(input$consultasSQLDF=="SQLDF11"){"Promedio de Componente del curso mateBasica"}
    else if(input$consultasSQLDF=="SQLDF12"){"Promedio de Componente de 1ciclo"}
    else if(input$consultasSQLDF=="SQLDF13"){"Promedio de Final del curso de IHM"}
    else if(input$consultasSQLDF=="SQLDF14"){"Top5 de Alumnos de 2ciclo con menos promedio promociona"}
    else if(input$consultasSQLDF=="SQLDF15"){"Top5 de alumnos de 3ciclo con más promedio promocional"}
    else if(input$consultasSQLDF=="SQLDF16"){"Desviacion estandar del 1componente del 4ciclo"}
    else if(input$consultasSQLDF=="SQLDF17"){"Cantidad de alumnos que dieron susti en 1 ciclo"}
    else if(input$consultasSQLDF=="SQLDF18"){"Cantidad de alumnos que dieron susti entre 1 ciclo y 8 ciclo"}
    else if(input$consultasSQLDF=="SQLDF19"){"Promedio de nota promocional de alumnos que estan por segunda en MPN"}
    else if(input$consultasSQLDF=="SQLDF20"){"Promedio de nota promocional de alumnos que estan por primera en MPN2"}
  })
  output$textoConsultasDT<-renderText({
    if(input$consultasDT=="DT1"){"Cantidad desaprobados en 4ciclo"}
    else if(input$consultasDT=="DT2"){"Cantidad inhabilitado en 1ciclo"}
    else if(input$consultasDT=="DT3"){"Cantidad aprobados en 6ciclo"}
    else if(input$consultasDT=="DT4"){"Promedio de notas en 5ciclo"}
    else if(input$consultasDT=="DT5"){"Cantidad de alumnos por segunda en el curso de MPN2"}
    else if(input$consultasDT=="DT6"){"Cantidad de alumnos por segunda en el 1ciclo"}
    else if(input$consultasDT=="DT7"){"Numero de alumnos con <=10 del curso ProyectoInvestigacion"}
    else if(input$consultasDT=="DT8"){"Numero de alumnos con 10<x=<15 del curso ProyectoInvestigacion"}
    else if(input$consultasDT=="DT9"){"Numero de alumnos con >15 del curso ProyectoInvestigacion"}
    else if(input$consultasDT=="DT10"){"Relacion entre consultas 7,8 y 9"}
    else if(input$consultasDT=="DT11"){"Numero de inhabilitados de cada curso en 2 ciclo"}
    else if(input$consultasDT=="DT12"){"Promedio de componentes del 1ciclo"}
    else if(input$consultasDT=="DT13"){"Promedio del Parcial del curso de ProyectoInvestigacion"}
    else if(input$consultasDT=="DT14"){"Cantidad de alumnos que no rindieron el examen final de ProyectInvestigacion"}
    else if(input$consultasDT=="DT15"){"¿Que promedio (final y parcial) del 8ciclo es mayor?"}
    else if(input$consultasDT=="DT16"){"Varianza de la nota promocional de 5ciclo"}
    else if(input$consultasDT=="DT17"){"Alumnos que desaprobaron el parcial pero aprobaron el final en IHM"}
    else if(input$consultasDT=="DT18"){"Alumnos que aprobaron el parcial pero desaprobaron el final en IHM"}
    else if(input$consultasDT=="DT19"){"Promedio de nota final de los cursos de la linea de programacion"}
    else if(input$consultasDT=="DT20"){"Varianza de la nota promocional de 5ciclo"}
  })
  
  #Logica del panel de "Graficos de Exploracion"
  output$graficosViewGG<-renderPlot({
    if(input$graficosGG=="GG1"){GG1}
    else if(input$graficosGG=="GG1"){GG1}
    else if(input$graficosGG=="GG2"){GG2}
    else if(input$graficosGG=="GG3"){GG3}
    else if(input$graficosGG=="GG4"){GG4}
    else if(input$graficosGG=="GG5"){GG5}
    else if(input$graficosGG=="GG6"){GG6}
    else if(input$graficosGG=="GG7"){GG7}
    else if(input$graficosGG=="GG8"){GG8}
    else if(input$graficosGG=="GG9"){GG9}
    else if(input$graficosGG=="GG10"){GG10}
    else if(input$graficosGG=="GG11"){GG11}
    else if(input$graficosGG=="GG12"){GG12}
    else if(input$graficosGG=="GG13"){GG13}
    else if(input$graficosGG=="GG14"){GG14}
    else if(input$graficosGG=="GG15"){GG15}
    else if(input$graficosGG=="GG16"){GG16}
    else if(input$graficosGG=="GG17"){GG17}
    else if(input$graficosGG=="GG19"){GG19}
    else if(input$graficosGG=="GG20"){GG20}
  })
  output$graficosViewPL<-renderPlotly({
    if(input$graficosPL=="plotly1"){plotly1}
    else if(input$graficosPL=="plotly2"){plotly2}
    else if(input$graficosPL=="plotly3"){plotly3}
    else if(input$graficosPL=="plotly4"){plotly4}
    else if(input$graficosPL=="plotly5"){plotly5}
    else if(input$graficosPL=="plotly6"){plotly6}
    else if(input$graficosPL=="plotly7"){plotly7}
    else if(input$graficosPL=="plotly8"){plotly8}
    else if(input$graficosPL=="plotly9"){plotly9}
    else if(input$graficosPL=="plotly10"){plotly10}
    else if(input$graficosPL=="plotly11"){plotly11}
    else if(input$graficosPL=="plotly12"){plotly12}
    else if(input$graficosPL=="plotly13"){plotly13}
    else if(input$graficosPL=="plotly14"){plotly14}
    else if(input$graficosPL=="plotly15"){plotly15}
    else if(input$graficosPL=="plotly16"){plotly16}
    else if(input$graficosPL=="plotly17"){plotly17}
    else if(input$graficosPL=="plotly18"){plotly18}
    else if(input$graficosPL=="plotly19"){plotly19}
    else if(input$graficosPL=="plotly20"){plotly20}
  })
  
  output$textoGraficosGG<-renderText({
    if(input$graficosGG=="GG1"){GG1}
    else if(input$graficosGG=="GG1"){"Join alumnos desaprobados en POO e IntroProg"}
    else if(input$graficosGG=="GG2"){"Top5 de Alumnos de 2ciclo con menos promedio promocional"}
    else if(input$graficosGG=="GG3"){"Cantidad de inhabilitados y habilitados del curso de mateBasica"}
    else if(input$graficosGG=="GG4"){"Cantidad de aprobados y desaprobados del 1°ciclo"}
    else if(input$graficosGG=="GG5"){"Cantidad de aprobados de IntroProg y POO"}
    else if(input$graficosGG=="GG6"){"Comparación de notas de la 1 mitad y 2 mitad del curso de mate3"}
    else if(input$graficosGG=="GG7"){"Top 3 de alumnos del 3ciclo"}
    else if(input$graficosGG=="GG8"){"top10 de alumnos IntroProg"}
    else if(input$graficosGG=="GG9"){"top10 de alumnos POO"}
    else if(input$graficosGG=="GG10"){"Alumnos top5 alumnos Sisope"}
    else if(input$graficosGG=="GG19"){"Desaprobados en AdmiRedes y ArquiRedes"}
    else if(input$graficosGG=="GG20"){"Aprobados en MateDiscreta y Mate3"}
    
  })
  output$textoGraficosPL<-renderText({
    if(input$graficosPL=="plotly1"){"Notas de EP y Final de alumnos de Fisica1"}
    else if(input$graficosPL=="plotly2"){"Notas del Final de alumnos del curso de fisica1"}
    else if(input$graficosPL=="plotly3"){"Notas del EP de alumnos del curso de fisica1"}
    else if(input$graficosPL=="plotly4"){"Join entre IntroProg y POO"}
    else if(input$graficosPL=="plotly5"){"Join entre Sisope y ArquiComp"}
    else if(input$graficosPL=="plotly6"){"Notas de alumnos de IntroIngSI"}
    else if(input$graficosPL=="plotly7"){"Notas de alumnos de MPN2"}
    else if(input$graficosPL=="plotly8"){"Notas de Peti y geti"}
    else if(input$graficosPL=="plotly9"){"Notas de los componente 1 y 4 de Sistema de Soporte de Decision"}
    else if(input$graficosPL=="plotly10"){"Notas de los componente 2 y 3 de Sistema de Soporte de Decision"}
    else if(input$graficosPL=="plotly19"){"Notas del C3 de Proyecto de Investigación"}
    else if(input$graficosPL=="plotly20"){"Notas del C4 de Proyecto de Investigación"}
  })
  
  #Logica del panel de "Modelos - Regresion Lineal"
  output$textoPearson<-renderText({
    M0mf<-MAlum(ICSI,input$curso1,input$curso2,input$notaX)
    M0mf[[3]]
  })
  output$textoNewY<-renderText({
    M0mf<-MAlum(ICSI,input$curso1,input$curso2,input$notaX)
    M0mf[[4]]
  })
  output$modeloView<-renderPlot({
    M0mf<-MAlum(ICSI,input$curso1,input$curso2,input$notaX)
    ggplot()+
      geom_point(data=M0mf[[5]],aes(x=x,y=y),color="blue")+
      geom_line(data=M0mf[[6]],aes(x=x,y=y),color="red")
  })
}


##-App-##
shinyApp(ui, server)