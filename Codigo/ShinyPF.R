#Add libraries
library("plotly")
library("shiny")
library("dplyr")
library("ggplot2")
library("readxl")
library("sqldf")
library("data.table")

consta=c("DPLYR1","DPLYR2","DPLYR3","DPLYR4","DPLYR5","DPLYR6","DPLYR7","DPLYR8","DPLYR9","DPLYR10",
         "DPLYR11","DPLYR12","DPLYR13","DPLYR14","DPLYR15","DPLYR16","DPLYR17","DPLYR18","DPLYR19","DPLYR20",
         "SQLDF1","SQLDF2","SQLDF3","SQLDF4","SQLDF5","SQLDF6","SQLDF7","SQLDF8","SQLDF9","SQLDF10",
         "SQLDF11","SQLDF12","SQLDF13","SQLDF14","SQLDF15","SQLDF16","SQLDF17","SQLDF18","SQLDF19","SQLDF20",
         "DT1","DT2","DT3","DT4","DT5","DT6","DT7","DT8","DT9","DT10","DT11","DT12","DT13","DT14",
         "DT15","DT16","DT17","DT18","DT19","DT20")

graf=c("GG1","GG2","GG3","GG4","GG5","GG6","GG7","GG8","GG9","GG10","GG11","GG12","GG13",
       "GG14","GG15","GG16","GG17","GG18","GG19","GG20","plotly1","plotly2","plotly3","plotly4",
       "plotly5","plotly6","plotly7","plotly8","plotly9","plotly10","plotly11","plotly12",
       "plotly13","plotly14","plotly15","plotly16","plotly17","plotly18","plotly19","plotly20")

cur<-c("Introduccion a la Ingenieria de Sistemas","Introduccion a la Programacion","Lenguaje","Matematica Basica",
       "Programacion Orientada a Objetos","Automatas y Compiladores","Calculo","Matematica Discreta",
       "Fisica 1","Matematica III","Fundamentos de Sistemas de Informacion","Algebra Lineal",
       "Fisica aplicada a la computacion","Organizacion y gestion de empresas","Estructura de datos","Arquitectura de computadoras",
       "Estadistica y Probabilidades","Base de datos","Interaccion Hombre Computador","Ingenieria de Software 1",
       "Desarrollo de Aplicaciones","Ingenieria de Software 2","Modelado de Proceso de Negocios 1","Sistema Gestor de Base de Datos","Sistemas Operativos",
       "Planeamiento Estrategico de TI","Investigacion de Operaciones","Sistemas de Informacion","Modelado de Proceso de Negocios 2","Arquitectura de Redes de Computadoras",
       "Administracion de Redes y Seguridad de la Informacion","Sistema de Soporte de Decision","Proyecto de Investigacion","Administacion de Proyectos de Sistemas de Informacion","Gestion de TI")

normal<-function(x){
  (x-min(x))/(max(x)-min(x))
}
dtNormal<-data.frame(ICSI%>%select(Final))
dtNormal<-normal(dtNormal$Final)
View(dtNormal)
#-Estructura de UI-#
ui<-fluidPage(
  titlePanel("Proyecto de Mainframes 1"),
  tabsetPanel(
    tabPanel("Recoleccion",
             sidebarLayout(
               sidebarPanel(
                 titlePanel("Ingreso de Datos"),
                 fileInput("idArchivo","Elija un archivo XLSX",accept=c(".xlsx")),
                 tags$hr(),
                 numericInput("idSheet",label="Ingrese el número de sheet",value=1,min=1,max=4)),
                 #actionButton("IdBoton",label="Ejecutar")),
               mainPanel(
                 tableOutput(outputId="plotRecoleccion")
               )
             )
             ),
    tabPanel("Pre-Procesamiento",
             titlePanel("Limpieza de Datos"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset","Elige un dataset",choices=c("IntroSI","IntroProg")),
                 actionButton("btnView","Ver"),
                 tags$hr(),
                 h3("Descripción"),
                 tags$ol(
                   tags$li("RawData -> Recolectamos la data cruda"), 
                   tags$li("AddCol -> Añadimos columas indicando nombre de curso y ciclo"), 
                   tags$li("CleanHead -> Eliminamos las filas innecesarias y agregamos cabecera"),
                   tags$li("FinalDataSet -> Unimos los 35 datasets en uno solo, convertimos las columas de character a numeric y eliminamos columnas con 'IN'")
                 )
                 ),
               
               mainPanel(
                 h4("Dataset"),
                 tabsetPanel(id="tabPanel",
                             tabPanel("RawData",tableOutput("view")),
                             tabPanel("AddCol",tableOutput("addCol")),
                             tabPanel("CleanHead",tableOutput("cleanHead")),
                             tabPanel("FinalDataset",tableOutput("finalDataset"))))
                 )
               ),
    tabPanel("Exploracion de datos",
             titlePanel("Consultas de Exploracion"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("consultas","Elige una consulta",choices=consta)
               ),
               mainPanel(
                 h4("Resultado de Consultas"),
                 textOutput("textoConsultas"),
                 tags$hr(),
                 tableOutput("consultasView")
               )
             )),
    tabPanel("Graficos de Exploracion",
             titlePanel("Graficos con Shiny y GGplot2"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("graficos","Elige un grafico",choices=graf)
               ),
               mainPanel(
                 h4("Resultado de los Graficos"),
                 textOutput("textoGraficos"),
                 tags$hr(),
                 plotOutput("graficosView"),
                 plotlyOutput("graficosViewP")
               )
             )),
    tabPanel("Modelo - Regresion Lineal",
             titlePanel("Modelo"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("curso1","Elige el primer curso",choices=cur),
                 selectInput("curso2","Elige el segundo curso",choices=cur),
                 numericInput("notaX",label="Ingrese nota",value=14,min=1,max=20)
               ),
               mainPanel(
                 h4("Grafico del Modelo"),
                 h5("Coeficiente de Pearson"),
                 textOutput("textoPearson"),
                 tags$hr(),
                 h5("Nuevo Valor de Y"),
                 textOutput("textoNewY"),
                 tags$hr(),
                 plotOutput("modeloView")
               )
             ))
  )
)

#-Estructura de servidor-#
server<-function(input,output){
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
  datasetInput<-eventReactive(input$btnView, {
    switch(input$dataset,
           "IntroSI" = raw1IntroIngSist,
           "IntroProg" = raw1IntroProg)
  }, ignoreNULL = FALSE)
  output$view<-renderTable({
    datasetInput()
  })
  raw1IntroIngSistM<-mutate(raw1IntroIngSist,Curso="Introduccion a la Ingenieria de Sistemas",Ciclo=1)
  output$addCol<-renderTable({raw1IntroIngSistM})
  raw1IntroIngSistMP<-ProcesarCurso(raw1IntroIngSistM)
  output$cleanHead<-renderTable({raw1IntroIngSistMP})
  output$finalDataset<-renderTable({ICSI})
  
  #Logica del panel de "Exploracion de Datos"
  output$consultasView<-renderTable({
    if(input$consultas=="DPLYR1"){DPLYR1}
    else if(input$consultas=="DPLYR2"){DPLYR2}
    else if(input$consultas=="DPLYR3"){DPLYR3}
    else if(input$consultas=="DPLYR4"){DPLYR4}
    else if(input$consultas=="DPLYR5"){DPLYR5}
    else if(input$consultas=="DPLYR6"){DPLYR6}
    else if(input$consultas=="DPLYR7"){DPLYR7}
    else if(input$consultas=="DPLYR8"){DPLYR8}
    else if(input$consultas=="DPLYR9"){DPLYR9}
    else if(input$consultas=="DPLYR10"){DPLYR10}
    else if(input$consultas=="DPLYR11"){DPLYR11}
    else if(input$consultas=="DPLYR12"){DPLYR12}
    else if(input$consultas=="DPLYR13"){DPLYR13}
    else if(input$consultas=="DPLYR14"){DPLYR14}
    else if(input$consultas=="DPLYR15"){DPLYR15}
    else if(input$consultas=="DPLYR16"){DPLYR16}
    else if(input$consultas=="DPLYR17"){DPLYR17}
    else if(input$consultas=="DPLYR18"){DPLYR18}
    else if(input$consultas=="DPLYR19"){DPLYR19}
    else if(input$consultas=="DPLYR20"){DPLYR20}
    else if(input$consultas=="SQLDF1"){SQL1}
    else if(input$consultas=="SQLDF2"){SQL2}
    else if(input$consultas=="SQLDF3"){SQL3}
    else if(input$consultas=="SQLDF4"){SQL4}
    else if(input$consultas=="SQLDF5"){SQL5}
    else if(input$consultas=="SQLDF6"){SQL6}
    else if(input$consultas=="SQLDF7"){SQL7}
    else if(input$consultas=="SQLDF8"){SQL8}
    else if(input$consultas=="SQLDF9"){SQL9}
    else if(input$consultas=="SQLDF10"){SQL10}
    else if(input$consultas=="SQLDF11"){SQL11}
    else if(input$consultas=="SQLDF12"){SQL12}
    else if(input$consultas=="SQLDF13"){SQL13}
    else if(input$consultas=="SQLDF14"){SQL14}
    else if(input$consultas=="SQLDF15"){SQL15}
    else if(input$consultas=="SQLDF16"){SQL16}
    else if(input$consultas=="SQLDF17"){SQL17}
    else if(input$consultas=="SQLDF18"){SQL18}
    else if(input$consultas=="SQLDF19"){SQL19}
    else if(input$consultas=="SQLDF20"){SQL20}
    else if(input$consultas=="DT1"){DT1}
    else if(input$consultas=="DT2"){DT2}
    else if(input$consultas=="DT3"){DT3}
    else if(input$consultas=="DT4"){DT4}
    else if(input$consultas=="DT5"){DT5}
    else if(input$consultas=="DT6"){DT6}
    else if(input$consultas=="DT7"){DT7}
    else if(input$consultas=="DT8"){DT8}
    else if(input$consultas=="DT9"){DT9}
    else if(input$consultas=="DT10"){DT10}
    else if(input$consultas=="DT11"){DT11}
    else if(input$consultas=="DT12"){DT12}
    else if(input$consultas=="DT13"){DT13}
    else if(input$consultas=="DT14"){DT14}
    else if(input$consultas=="DT15"){DT15}
    else if(input$consultas=="DT16"){DT16}
    else if(input$consultas=="DT17"){DT17}
    else if(input$consultas=="DT18"){DT18}
    else if(input$consultas=="DT19"){DT19}
    else if(input$consultas=="DT20"){DT20}
  })
  output$textoConsultas<-renderText({
    if(input$consultas=="DPLYR1"){"inhabilitados y habilitados en mateBasica"}
    else if(input$consultas=="DPLYR2"){"aprobados y desaprobados en 1°ciclo"}
    else if(input$consultas=="DPLYR3"){"aprobados de IntroProg y POO"}
    else if(input$consultas=="DPLYR4"){"Nota media del curso de calculo"}
    else if(input$consultas=="DPLYR5"){"Varianza en el parcial de Lenguaje"}
    else if(input$consultas=="DPLYR6"){"Promedio de notas hasta el parcial de mate3"}
    else if(input$consultas=="DPLYR7"){"Promedio de nota Final curso de mate3"}
    else if(input$consultas=="DPLYR8"){"Comparación de notas de la 1 y 2 mitad de mate3"}
    else if(input$consultas=="DPLYR9"){"Notas de alumnos del curso de fisica1"}
    else if(input$consultas=="DPLYR10"){"alumnos que están por encima de la media del curso de EstruDatos"}
    else if(input$consultas=="DPLYR11"){"Top 3 de alumnos del 3ciclo"}
    else if(input$consultas=="DPLYR12"){"Desviacion Estandar del examen final de FisCom"}
    else if(input$consultas=="DPLYR13"){"Alumno con mayor promedio de 1ciclo"}
    else if(input$consultas=="DPLYR14"){"top10 de alumnos IntroProg"}
    else if(input$consultas=="DPLYR15"){"top10 de alumnos POO"}
    else if(input$consultas=="DPLYR16"){"Join entre IntroProg y POO"}
    else if(input$consultas=="DPLYR17"){"Alumnos top5 alumnos Sisope"}
    else if(input$consultas=="DPLYR18"){"Alumnos top5 alumnos ArquiComp"}
    else if(input$consultas=="DPLYR19"){"Join entre Sisope y ArquiComp"}
    else if(input$consultas=="DPLYR20"){"Cantidad entre aprobados de MPN1 y MPN2"}
    else if(input$consultas=="SQLDF1"){"aprobados y desaprobados en IntroIngSI"}
    else if(input$consultas=="SQLDF2"){"aprobados en el curso de IntroProg"}
    else if(input$consultas=="SQLDF3"){"Alumnos que no están invictos hasta 3 ciclo"}
    else if(input$consultas=="SQLDF4"){"Alumnos desaprobados en IntroProg"}
    else if(input$consultas=="SQLDF5"){"Alumnos desaprobados en MateBasica"}
    else if(input$consultas=="SQLDF6"){"Join alumnos desaprobados en POO e IntroProg"}
    else if(input$consultas=="SQLDF7"){"Relacion de alumnos de 1ciclo y 7ciclo"}
    else if(input$consultas=="SQLDF8"){"Cantidad de registros de alumnos desaprobados por ciclo"}
    else if(input$consultas=="SQLDF9"){"Cantidad de registros de alumnos por ciclo"}
    else if(input$consultas=="SQLDF10"){"Tasa de desaprobació de ICSI según ciclo en el semestre 201510"}
    else if(input$consultas=="SQLDF11"){"Promedio de Componente del curso mateBasica"}
    else if(input$consultas=="SQLDF12"){"Promedio de Componente de 1ciclo"}
    else if(input$consultas=="SQLDF13"){"Promedio de Final del curso de IHM"}
    else if(input$consultas=="SQLDF14"){"Top5 de Alumnos de 2ciclo con menos promedio promociona"}
    else if(input$consultas=="SQLDF15"){"Top5 de alumnos de 3ciclo con más promedio promocional"}
    else if(input$consultas=="SQLDF16"){"Desviacion estandar del 1componente del 4ciclo"}
    else if(input$consultas=="SQLDF17"){"Cantidad de alumnos que dieron susti en 1 ciclo"}
    else if(input$consultas=="SQLDF18"){"Cantidad de alumnos que dieron susti entre 1 ciclo y 8 ciclo"}
    else if(input$consultas=="SQLDF19"){"Promedio de nota promocional de alumnos que estan por segunda en MPN"}
    else if(input$consultas=="SQLDF20"){"Promedio de nota promocional de alumnos que estan por primera en MPN2"}
    else if(input$consultas=="DT1"){"Cantidad desaprobados en 4ciclo"}
    else if(input$consultas=="DT2"){"Cantidad inhabilitado en 1ciclo"}
    else if(input$consultas=="DT3"){"Cantidad aprobados en 6ciclo"}
    else if(input$consultas=="DT4"){"Promedio de notas en 5ciclo"}
    else if(input$consultas=="DT5"){"Cantidad de alumnos por segunda en el curso de MPN2"}
    else if(input$consultas=="DT6"){"Cantidad de alumnos por segunda en el 1ciclo"}
    else if(input$consultas=="DT7"){"Numero de alumnos con <=10 del curso ProyectoInvestigacion"}
    else if(input$consultas=="DT8"){"Numero de alumnos con 10<x=<15 del curso ProyectoInvestigacion"}
    else if(input$consultas=="DT9"){"Numero de alumnos con >15 del curso ProyectoInvestigacion"}
    else if(input$consultas=="DT10"){"Relacion entre consultas 7,8 y 9"}
    else if(input$consultas=="DT11"){"Numero de inhabilitados de cada curso en 2 ciclo"}
    else if(input$consultas=="DT12"){"Promedio de componentes del 1ciclo"}
    else if(input$consultas=="DT13"){"Promedio del Parcial del curso de ProyectoInvestigacion"}
    else if(input$consultas=="DT14"){"Cantidad de alumnos que no rindieron el examen final de ProyectInvestigacion"}
    else if(input$consultas=="DT15"){"¿Que promedio (final y parcial) del 8ciclo es mayor?"}
    else if(input$consultas=="DT16"){"Varianza de la nota promocional de 5ciclo"}
    else if(input$consultas=="DT17"){"Alumnos que desaprobaron el parcial pero aprobaron el final en IHM"}
    else if(input$consultas=="DT18"){"Alumnos que aprobaron el parcial pero desaprobaron el final en IHM"}
    else if(input$consultas=="DT19"){"Promedio de nota final de los cursos de la linea de programacion"}
    else if(input$consultas=="DT20"){"Varianza de la nota promocional de 5ciclo"}
    })
  
  #Logica del panel de "Graficos de Exploracion"
  output$graficosView<-renderPlot({
    if(input$graficos=="GG1"){GG1}
    else if(input$graficos=="GG1"){GG1}
    else if(input$graficos=="GG2"){GG2}
    else if(input$graficos=="GG3"){GG3}
    else if(input$graficos=="GG4"){GG4}
    else if(input$graficos=="GG5"){GG5}
    else if(input$graficos=="GG6"){GG6}
    else if(input$graficos=="GG7"){GG7}
    else if(input$graficos=="GG8"){GG8}
    else if(input$graficos=="GG9"){GG9}
    else if(input$graficos=="GG10"){GG10}
    else if(input$graficos=="GG11"){GG11}
    else if(input$graficos=="GG12"){GG12}
    else if(input$graficos=="GG13"){GG13}
    else if(input$graficos=="GG14"){GG14}
    else if(input$graficos=="GG15"){GG15}
    else if(input$graficos=="GG16"){GG16}
    else if(input$graficos=="GG17"){GG17}
    else if(input$graficos=="GG19"){GG19}
    else if(input$graficos=="GG20"){GG20}
    })
  output$graficosViewP<-renderPlotly({
    if(input$graficos=="plotly1"){plotly1}
    else if(input$graficos=="plotly2"){plotly2}
    else if(input$graficos=="plotly3"){plotly3}
    else if(input$graficos=="plotly4"){plotly4}
    else if(input$graficos=="plotly5"){plotly5}
    else if(input$graficos=="plotly6"){plotly6}
    else if(input$graficos=="plotly7"){plotly7}
    else if(input$graficos=="plotly8"){plotly8}
    else if(input$graficos=="plotly9"){plotly9}
    else if(input$graficos=="plotly10"){plotly10}
    else if(input$graficos=="plotly11"){plotly11}
    else if(input$graficos=="plotly12"){plotly12}
    else if(input$graficos=="plotly13"){plotly13}
    else if(input$graficos=="plotly14"){plotly14}
    else if(input$graficos=="plotly15"){plotly15}
    else if(input$graficos=="plotly16"){plotly16}
    else if(input$graficos=="plotly17"){plotly17}
    else if(input$graficos=="plotly18"){plotly18}
    else if(input$graficos=="plotly19"){plotly19}
    else if(input$graficos=="plotly20"){plotly20}
  })
  output$textoGraficos<-renderText({
    if(input$graficos=="GG1"){GG1}
    else if(input$graficos=="GG1"){"Join alumnos desaprobados en POO e IntroProg"}
    else if(input$graficos=="GG2"){"Top5 de Alumnos de 2ciclo con menos promedio promocional"}
    else if(input$graficos=="GG3"){"Cantidad de inhabilitados y habilitados del curso de mateBasica"}
    else if(input$graficos=="GG4"){"Cantidad de aprobados y desaprobados del 1°ciclo"}
    else if(input$graficos=="GG5"){"Cantidad de aprobados de IntroProg y POO"}
    else if(input$graficos=="GG6"){"Comparación de notas de la 1 mitad y 2 mitad del curso de mate3"}
    else if(input$graficos=="GG7"){"Top 3 de alumnos del 3ciclo"}
    else if(input$graficos=="GG8"){"top10 de alumnos IntroProg"}
    else if(input$graficos=="GG9"){"top10 de alumnos POO"}
    else if(input$graficos=="GG10"){"Alumnos top5 alumnos Sisope"}
    else if(input$graficos=="GG19"){"Desaprobados en AdmiRedes y ArquiRedes"}
    else if(input$graficos=="GG20"){"Aprobados en MateDiscreta y Mate3"}
    else if(input$graficos=="plotly1"){"Notas del EP de alumnos del curso de fisica1"}
    else if(input$graficos=="plotly2"){"Notas del Final de alumnos del curso de fisica1"}
    else if(input$graficos=="plotly3"){"Notas de EP y Final de alumnos de Fisica1"}
    else if(input$graficos=="plotly4"){"Join entre IntroProg y POO"}
    else if(input$graficos=="plotly5"){"Join entre Sisope y ArquiComp"}
    else if(input$graficos=="plotly6"){"Notas de alumnos de IntroIngSI"}
    else if(input$graficos=="plotly7"){"Notas de alumnos de MPN2"}
    else if(input$graficos=="plotly8"){"Notas de Peti y geti"}
    else if(input$graficos=="plotly9"){"Notas de los componente 1 y 4 de Sistema de Soporte de Decision"}
    else if(input$graficos=="plotly10"){"Notas de los componente 2 y 3 de Sistema de Soporte de Decision"}
    else if(input$graficos=="plotly19"){"Notas del C3 de Proyecto de Investigación"}
    else if(input$graficos=="plotly20"){"Notas del C4 de Proyecto de Investigación"}
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

#-Run the Application-#
shinyApp(ui,server)