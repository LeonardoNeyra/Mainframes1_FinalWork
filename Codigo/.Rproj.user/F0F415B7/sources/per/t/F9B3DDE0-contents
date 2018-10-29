#Packages
library("readxl")
library("ggplot2")
library("dplyr")
library("janitor")
library("sqldf")
library("data.table")
library("plotly")

#Recolección de Datos

##Primer Ciclo
raw1IntroIngSI<-read_xlsx("Datasets/Ciclo1Primer.xlsx",sheet = 1)
raw1IntroProg<-read_xlsx("Datasets/Ciclo1Primer.xlsx",sheet = "IntroProg")
raw1MateBasica<-read_xlsx("Datasets/Ciclo1Primer.xlsx",sheet = "MateBasica")
raw1Lenguaje<-read_xlsx("Datasets/Ciclo1Primer.xlsx",sheet = "Lenguaje")

raw1IntroIngSI<-mutate(raw1IntroIngSI,Curso="Introduccion a la Ingenieria de Sistemas",Ciclo=1)
raw1IntroProg<-mutate(raw1IntroProg,Curso="Introduccion a la Programacion",Ciclo=1)
raw1Lenguaje<-mutate(raw1Lenguaje,Curso="Lenguaje",Ciclo=1)
raw1MateBasica<-mutate(raw1MateBasica,Curso="Matematica Basica",Ciclo=1)

##Segundo Ciclo
raw2POO<-read_xlsx("Datasets/Ciclo2Segundo.xlsx",sheet = "POO")
raw2AutComp<-read_xlsx("Datasets/Ciclo2Segundo.xlsx",sheet = "AutComp")
raw2Calculo<-read_xlsx("Datasets/Ciclo2Segundo.xlsx",sheet = "Calculo")
raw2MateDiscreta<-read_xlsx("Datasets/Ciclo2Segundo.xlsx",sheet = "MateDiscreta")

raw2POO<-mutate(raw2POO,Curso="Programacion Orientada a Objetos",Ciclo=2)
raw2AutComp<-mutate(raw2AutComp,Curso="Automatas y Compiladores",Ciclo=2)
raw2Calculo<-mutate(raw2Calculo,Curso="Calculo",Ciclo=2)
raw2MateDiscreta<-mutate(raw2MateDiscreta,Curso="Matematica Discreta",Ciclo=2)

##Tercer Ciclo
raw3AlgebraLineal<-read_xlsx("Datasets/Ciclo3Tercero.xlsx",sheet = "AlgebraLineal")
raw3FunSI<-read_xlsx("Datasets/Ciclo3Tercero.xlsx",sheet = "FunSI")
raw3Mate3<-read_xlsx("Datasets/Ciclo3Tercero.xlsx",sheet = "Mate3")
raw3Fisica1<-read_xlsx("Datasets/Ciclo3Tercero.xlsx",sheet = "Fisica1")

raw3Fisica1<-mutate(raw3Fisica1,Curso="Fisica 1",Ciclo=3)
raw3Mate3<-mutate(raw3Mate3,Curso="Matematica III",Ciclo=3)
raw3FunSI<-mutate(raw3FunSI,Curso="Fundamentos de Sistemas de Informacion",Ciclo=3)
raw3AlgebraLineal<-mutate(raw3AlgebraLineal,Curso="Algebra Lineal",Ciclo=3)

##Cuarto Ciclo
raw4ArquiComp<-read_xlsx("Datasets/Ciclo4Cuarto.xlsx",sheet = "ArquiComp")
raw4EstruDatos<-read_xlsx("Datasets/Ciclo4Cuarto.xlsx",sheet = "EstruDatos")
raw4OrgaGest<-read_xlsx("Datasets/Ciclo4Cuarto.xlsx",sheet = "OrgaGest")
raw4FisCom<-read_xlsx("Datasets/Ciclo4Cuarto.xlsx",sheet = "FisCom")

raw4FisCom<-mutate(raw4FisCom,Curso="Fisica aplicada a la computacion",Ciclo=4)
raw4OrgaGest<-mutate(raw4OrgaGest,Curso="Organizacion y gestion de empresas",Ciclo=4)
raw4EstruDatos<-mutate(raw4EstruDatos,Curso="Estructura de datos",Ciclo=4)
raw4ArquiComp<-mutate(raw4ArquiComp,Curso="Arquitectura de computadoras",Ciclo=4)

##Quinto Ciclo
raw5INSO1<-read_xlsx("Datasets/Ciclo5Quinto.xlsx",sheet = "INSO1")
raw5IHM<-read_xlsx("Datasets/Ciclo5Quinto.xlsx",sheet = "IHM")
raw5BD<-read_xlsx("Datasets/Ciclo5Quinto.xlsx",sheet = "BD")
raw5ESTPROB<-read_xlsx("Datasets/Ciclo5Quinto.xlsx",sheet = "ESTPROB")

raw5ESTPROB<-mutate(raw5ESTPROB,Curso="Estadistica y Probabilidades",Ciclo=5)
raw5BD<-mutate(raw5BD,Curso="Base de datos",Ciclo=5)
raw5IHM<-mutate(raw5IHM,Curso="Interaccion Hombre Computador",Ciclo=5)
raw5INSO1<-mutate(raw5INSO1,Curso="Ingenieria de Software 1",Ciclo=5)

##Sexto Ciclo
raw6DAP<-read_xlsx("Datasets/Ciclo6Sexto.xlsx",sheet = "DAP")
raw6MPN1<-read_xlsx("Datasets/Ciclo6Sexto.xlsx",sheet = "MPN1")
raw6SISOPE<-read_xlsx("Datasets/Ciclo6Sexto.xlsx",sheet = "SISOPE")
raw6SGBD<-read_xlsx("Datasets/Ciclo6Sexto.xlsx",sheet = "SGBD")
raw6INSO2<-read_xlsx("Datasets/Ciclo6Sexto.xlsx",sheet = "INSO2")

raw6DAP<-mutate(raw6DAP,Curso="Desarrollo de Aplicaciones",Ciclo=6)
raw6INSO2<-mutate(raw6INSO2,Curso="Ingenieria de Software 2",Ciclo=6)
raw6MPN1<-mutate(raw6MPN1,Curso="Modelado de Proceso de Negocios 1",Ciclo=6)
raw6SGBD<-mutate(raw6SGBD,Curso="Sistema Gestor de Base de Datos",Ciclo=6)
raw6SISOPE<-mutate(raw6SISOPE,Curso="Sistemas Operativos",Ciclo=6)

##Septimo Ciclo
raw7ARQRC<-read_xlsx("Datasets/Ciclo7Septimo.xlsx",sheet = "ARQRC")
raw7MPN2<-read_xlsx("Datasets/Ciclo7Septimo.xlsx",sheet = "MPN2")
raw7SISTINFO<-read_xlsx("Datasets/Ciclo7Septimo.xlsx",sheet = "SISTINFO")
raw7INVOPE<-read_xlsx("Datasets/Ciclo7Septimo.xlsx",sheet = "INVOPE")
raw7PETI<-read_xlsx("Datasets/Ciclo7Septimo.xlsx",sheet = "PETI")

raw7PETI<-mutate(raw7PETI,Curso="Planeamiento Estrategico de TI",Ciclo=7)
raw7INVOPE<-mutate(raw7INVOPE,Curso="Investigacion de Operaciones",Ciclo=7)
raw7SISTINFO<-mutate(raw7SISTINFO,Curso="Sistemas de Informacion",Ciclo=7)
raw7MPN2<-mutate(raw7MPN2,Curso="Modelado de Proceso de Negocios 2",Ciclo=7)
raw7ARQRC<-mutate(raw7ARQRC,Curso="Arquitectura de Redes de Computadoras",Ciclo=7)

##Octavo Ciclo
raw8ADMRS<-read_xlsx("Datasets/Ciclo8Octavo.xlsx",sheet = "ADMRS")
raw8GETI<-read_xlsx("Datasets/Ciclo8Octavo.xlsx",sheet = "GETI")
raw8APSI<-read_xlsx("Datasets/Ciclo8Octavo.xlsx",sheet = "APSI")
raw8PROINV<-read_xlsx("Datasets/Ciclo8Octavo.xlsx",sheet = "PROINV")
raw8SSD<-read_xlsx("Datasets/Ciclo8Octavo.xlsx",sheet = "SSD")

raw8ADMRS<-mutate(raw8ADMRS,Curso="Administracion de Redes y Seguridad de la Informacion",Ciclo=8)
raw8SSD<-mutate(raw8SSD,Curso="Sistema de Soporte de Decision",Ciclo=8)
raw8PROINV<-mutate(raw8PROINV,Curso="Proyecto de Investigacion",Ciclo=8)
raw8APSI<-mutate(raw8APSI,Curso="Administacion de Proyectos de Sistemas de Informacion",Ciclo=8)
raw8GETI<-mutate(raw8GETI,Curso="Gestion de TI",Ciclo=8)

#Preprocesamiento de Datos rawICSI
ProcesarCurso<-function(dt){
  ##Limpieza de datos
  colnames(dt)=c("nro","Vez","Codigo","ApellidoNombre","C1","C2","EP","C3","C4","EF","Prom","Sust","Final","Curso","Ciclo")
  dt<-dt[-c(1:7),]
  dt<-dt[,-1]
  dt<-dt[,-3]

  ##Imputacion de datos
  for (i in 1:NROW(dt)) {
    if(is.na(dt$Sust[i])==TRUE){
      dt$Sust[i]<-"ND"
    }
  }
  return(dt)
}

##Normalizar
###1 ciclo
raw1IntroIngSI<-ProcesarCurso(raw1IntroIngSI)
raw1IntroProg<-ProcesarCurso(raw1IntroProg)
raw1Lenguaje<-ProcesarCurso(raw1Lenguaje)
raw1MateBasica<-ProcesarCurso(raw1MateBasica)

###2 ciclo
raw2POO<-ProcesarCurso(raw2POO)
raw2AutComp<-ProcesarCurso(raw2AutComp)
raw2Calculo<-ProcesarCurso(raw2Calculo)
raw2MateDiscreta<-ProcesarCurso(raw2MateDiscreta)

###3 ciclo
raw3Fisica1<-ProcesarCurso(raw3Fisica1)
raw3Mate3<-ProcesarCurso(raw3Mate3)
raw3FunSI<-ProcesarCurso(raw3FunSI)
raw3AlgebraLineal<-ProcesarCurso(raw3AlgebraLineal)

###4 ciclo
raw4FisCom<-ProcesarCurso(raw4FisCom)
raw4OrgaGest<-ProcesarCurso(raw4OrgaGest)
raw4EstruDatos<-ProcesarCurso(raw4EstruDatos)
raw4ArquiComp<-ProcesarCurso(raw4ArquiComp)

###5 ciclo
raw5ESTPROB<-ProcesarCurso(raw5ESTPROB)
raw5BD<-ProcesarCurso(raw5BD)
raw5IHM<-ProcesarCurso(raw5IHM)
raw5INSO1<-ProcesarCurso(raw5INSO1)

###6 ciclo
raw6DAP<-ProcesarCurso(raw6DAP)
raw6INSO2<-ProcesarCurso(raw6INSO2)
raw6MPN1<-ProcesarCurso(raw6MPN1)
raw6SGBD<-ProcesarCurso(raw6SGBD)
raw6SISOPE<-ProcesarCurso(raw6SISOPE)

###7 ciclo
raw7PETI<-ProcesarCurso(raw7PETI)
raw7INVOPE<-ProcesarCurso(raw7INVOPE)
raw7SISTINFO<-ProcesarCurso(raw7SISTINFO)
raw7MPN2<-ProcesarCurso(raw7MPN2)
raw7ARQRC<-ProcesarCurso(raw7ARQRC)

###8 ciclo
raw8ADMRS<-ProcesarCurso(raw8ADMRS)
raw8SSD<-ProcesarCurso(raw8SSD)
raw8PROINV<-ProcesarCurso(raw8PROINV)
raw8APSI<-ProcesarCurso(raw8APSI)
raw8GETI<-ProcesarCurso(raw8GETI)

#Transformacion
#Totalizado con IN

rawICSI<-bind_rows(raw1IntroIngSI,raw1IntroProg,raw1Lenguaje,raw1MateBasica,
                   raw2AutComp,raw2Calculo,raw2MateDiscreta,raw2POO,
                   raw3AlgebraLineal,raw3Fisica1,raw3FunSI,raw3Mate3,
                   raw4ArquiComp,raw4EstruDatos,raw4FisCom,raw4OrgaGest,
                   raw5BD,raw5ESTPROB,raw5IHM,raw5INSO1,
                   raw6DAP,raw6INSO2,raw6MPN1,raw6SGBD,raw6SISOPE,
                   raw7ARQRC,raw7INVOPE,raw7MPN2,raw7PETI,raw7SISTINFO,
                   raw8ADMRS,raw8APSI,raw8GETI,raw8PROINV,raw8SSD)

##Eliminar IN
  DatosIN<-rawICSI %>% filter(Final == "IN")
  ICSI <- anti_join(rawICSI,DatosIN)
  ICSI[,3:9]<-as.numeric(unlist(ICSI[,3:9]))
  ICSI[,11]<-as.numeric(unlist(ICSI[,11]))
  ICSI<-adorn_rounding(dat = ICSI, digits = 0, rounding = "half up", skip_first_col = TRUE)

#Consultas y Exploración
##SQLDF
  #1.Cantidad de desaprobados en el curso de IntroIngSI 
  SQL1<-sqldf("select count(*) as desap from ICSI where Curso='Introduccion a la Ingenieria de Sistemas' and FInal<=10")
  #2.Cantidad de aprobados en el curso de IntroProg
  SQL2<-sqldf("select count(*) from ICSI where Curso='Introduccion a la Programacion' and FInal>10")
  #3.Alumnos que no están invictos hasta 3 ciclo
  SQL3<-sqldf("select Codigo from ICSI where Ciclo<=3 and Final<11")
  #4.Alumnos desaprobados en IntroProg
  SQL4<-sqldf("select Codigo,Final from ICSI where Curso='Introduccion a la Programacion' and Final<=10" ) 
  #5.Alumnos desaprobados en MateBasica
  SQL5<-sqldf("select Codigo,Final from ICSI where Curso='Matematica Basica' and Final<=10" ) 
  #6.Join alumnos desaprobados en POO e IntroProg
  SQL6<-sqldf("select IP.codigo as codIntro, IP.Final as FinalIntro, POO.codigo as codPoo, POO.Final as FinalPoo
              from SQL4 as IP inner join SQL5 as POO on IP.Codigo=POO.COdigo")
  #7.Relacion de alumnos de 1ciclo y 7ciclo 
  SQL7<-(sqldf("select count(distinct Codigo) from ICSI where Ciclo=7 "))/(sqldf("select count(distinct Codigo) from ICSI where Ciclo=1 "))
  #8.Cantidad de registros de alumnos desaprobados por ciclo
  SQL8.1<-sqldf("select count(*) as primer from ICSI where Ciclo=1 and FInal<=10")
  SQL8.2<-sqldf("select count(*) as segund from ICSI where Ciclo=2 and FInal<=10")
  SQL8.3<-sqldf("select count(*) as tercer from ICSI where Ciclo=3 and FInal<=10")
  SQL8.4<-sqldf("select count(*) as cuarto from ICSI where Ciclo=4 and FInal<=10")
  SQL8.5<-sqldf("select count(*) as quinto from ICSI where Ciclo=5 and FInal<=10")
  SQL8.6<-sqldf("select count(*) as sexto from ICSI where Ciclo=6 and FInal<=10")
  SQL8.7<-sqldf("select count(*) as septim from ICSI where Ciclo=7 and FInal<=10")
  SQL8.8<-sqldf("select count(*) as octavo from ICSI where Ciclo=8 and FInal<=10")
  SQL8<-bind_cols(SQL8.1,SQL8.2,SQL8.3,SQL8.4,SQL8.5,SQL8.6,SQL8.7,SQL8.8)
  #9.Cantidad de registros de alumnos por ciclo
  SQL9.1<-sqldf("select count(*) as primer from ICSI where Ciclo=1")
  SQL9.2<-sqldf("select count(*) as segund from ICSI where Ciclo=2")
  SQL9.3<-sqldf("select count(*) as tercer from ICSI where Ciclo=3")
  SQL9.4<-sqldf("select count(*) as cuarto from ICSI where Ciclo=4")
  SQL9.5<-sqldf("select count(*) as quinto from ICSI where Ciclo=5")
  SQL9.6<-sqldf("select count(*) as sexto from ICSI where Ciclo=6")
  SQL9.7<-sqldf("select count(*) as septim from ICSI where Ciclo=7")
  SQL9.8<-sqldf("select count(*) as octavo from ICSI where Ciclo=8")
  SQL9<-bind_cols(SQL9.1,SQL9.2,SQL9.3,SQL9.4,SQL9.5,SQL9.6,SQL9.7,SQL9.8)
  #10. Tasa de desaprobació de ICSI según ciclo en el semestre 201510
  SQL10.1<-(SQL9.1/SQL8.1)
  SQL10.2<-(SQL9.2/SQL8.2)
  SQL10.3<-(SQL9.3/SQL8.3)
  SQL10.4<-(SQL9.4/SQL8.4)
  SQL10.5<-(SQL9.5/SQL8.5)
  SQL10.6<-(SQL9.6/SQL8.6)
  SQL10.7<-(SQL9.7/SQL8.7)
  SQL10.8<-(SQL9.8/SQL8.8)
  SQL10<-bind_cols(SQL10.1,SQL10.2,SQL10.3,SQL10.4,SQL10.5,SQL10.6,SQL10.7,SQL10.8)
  #11.Promedio de Componente del curso mateBasica 
  SQL11<-sqldf("select avg(C1),avg(C2),avg(C3),avg(C4) from ICSI where Curso='Matematica Basica'")
  #12.Promedio de Componente de 1ciclo 
  SQL12<-sqldf("select avg(C1),avg(C2),avg(C3),avg(C4) from ICSI where Ciclo=1")
  #13.Promedio de Final del curso de IHM 
  SQL13<-sqldf("select avg(Final) from ICSI where Curso='Interaccion Hombre Computador'")
  #14.Top5 de Alumnos de 2ciclo con menos promedio promocional
  SQL14<-sqldf("Select Codigo, Prom from ICSI where Ciclo=2 group by Codigo order by Prom ASC LIMIT 5")
  #15.Top5 de alumnos de 3ciclo con más promedio promocional 
  SQL15<-sqldf("Select Codigo , Prom from ICSI where Ciclo=3 group by Codigo order by Prom DESC LIMIT 5")
  #16.Desviacion estandar del 1componente del 4ciclo
  SQL16<-sqldf("Select stdev(C1) from ICSI where Ciclo=4")
  #17.Cantidad de alumnos que dieron susti en 1 ciclo
  SQL17<-sqldf("Select count(*) from rawICSI where Ciclo='1' and Sust!='ND'")
  #18.Cantidad de alumnos que dieron susti entre 1 ciclo y 8 ciclo
  SQL18<-sqldf("Select count(*) from rawICSI where Sust!='ND'")
  #19.Promedio de nota promocional de alumnos que estan por segunda en MPN
  SQL19<-sqldf("Select avg(Prom) from ICSI where Curso='Modelado de Proceso de Negocios 1' and Vez=2")
  #20.Promedio de nota promocional de alumnos que estan por primera en MPN2
  SQL20<-sqldf("Select avg(Prom) from ICSI where Curso='Modelado de Proceso de Negocios 2' and Vez=1")
  
##DPLYR
  #1.Cantidad de inhabilitados y habilitados del curso de mateBasica 
  DPLYR1.1<-rawICSI %>% filter(Final=='IN',Curso=='Matematica Basica') %>% summarise(n()) %>% mutate(Tipo="IN")
  DPLYR1.2<-rawICSI %>% filter(Final!='IN',Curso=='Matematica Basica') %>% summarise(n()) %>% mutate(Tipo="HAB")
  DPLYR1<-bind_rows(DPLYR1.1,DPLYR1.2)
  #2.Cantidad de aprobados y desaprobados del 1°ciclo 
  DPLYR2.1<-ICSI %>% filter(Final>11,Ciclo==1) %>% summarise(n()) %>% mutate(Tipo="Aprobado")
  DPLYR2.2<-ICSI %>% filter(Final<11,Ciclo==1) %>% summarise(n()) %>% mutate(Tipo="Desaprobado")
  DPLYR2<-bind_rows(DPLYR2.1,DPLYR2.2)
  #3.Cantidad de aprobados de IntroProg y POO
  DPLYR3.1<-ICSI%>%filter(Final>10,Curso=='Introduccion a la Programacion')%>% summarise(n()) %>% mutate(Tipo="IntroProg")
  DPLYR3.2<-ICSI%>%filter(Final>10,Curso=='Programacion Orientada a Objetos')%>% summarise(n()) %>% mutate(Tipo="Poo")
  DPLYR3<-bind_rows(DPLYR3.1,DPLYR3.2)
  #4.Nota media del curso de calculo
  DPLYR4<-ICSI%>%filter(Curso=='Calculo') %>% summarize(mean(Final))
  #5.Varianza en el parcial de Lenguaje y Comunicación
  DPLYR5<-ICSI %>% summarise(var(Curso=='Lenguaje'))
  #6.Promedio de notas hasta el parcial del curso de mate3
  DPLYR6<-ICSI %>% filter(Curso=='Matematica III') %>% select(C1,C2) %>% summarize(Promedio=mean(mean(C1),mean(C2))) %>%
    mutate(Tipo="PrimeraMitad")
  #7.Promedio de nota hasta el final del curso de mate3
  DPLYR7<-ICSI %>% filter(Curso=='Matematica III') %>% select(C3,C4) %>% summarise(Promedio=mean(mean(C3),mean(C4))) %>%
    mutate(Tipo="SegundaMitad")
  #8.Comparación de notas de la 1 mitad y 2 mitad del curso de mate3
  DPLYR8<-bind_rows(DPLYR6,DPLYR7)
  #9.Notas de alumnos del curso de fisica1 (x=componentes,y=alumnos)
  DPLYR9<-ICSI %>% filter(Curso=="Fisica 1")%>%select(Codigo,C1,C2,EP,C3,C4,Prom,Final)
  #10.Cantidad de alumnos que están por encima de la media del curso de EstruDatos
  mediaEstruDatos<-as.numeric(ICSI%>%summarise(mean(Final)))
  DPLYR10<-ICSI%>%filter(Curso=="Estructura de datos")%>% filter(Final>=mediaEstruDatos)
  #11.Top 3 de alumnos del 3ciclo
  DPLYR11<-ICSI%>%filter(Ciclo==3)%>%group_by(Codigo)%>%summarise(Prom=mean(Final))%>%arrange(desc(Prom))%>%slice(1:3)
  #12.Desviacion Estandar del examen final de FisCom
  DPLYR12<-ICSI%>%filter(Curso=="Fisica aplicada a la computacion")%>%summarise(sd(Final))
  #13.Alumno con mayor promedio de 1ciclo
  DPLYR13<-ICSI%>%filter(Ciclo==1)%>%group_by(Codigo)%>%summarise(mean(Final))%>%top_n(1)
  #14.top10 de alumnos IntroProg
  DPLYR14<-ICSI%>%filter(Curso=="Introduccion a la Programacion")%>%select(Codigo,Final)%>%arrange(desc(Final))%>%slice(1:10)
  #15.top10 de alumnos POO
  DPLYR15<-ICSI%>%filter(Curso=="Programacion Orientada a Objetos")%>%select(Codigo,Final)%>%arrange(desc(Final))%>%slice(1:10)
  #16.Join entre IntroProg y POO
  DPLYR16<-inner_join(DPLYR14,DPLYR15,by="Codigo")
  #17.Alumnos top5 alumnos Sisope
  DPLYR17<-ICSI%>%filter(Curso=="Sistemas Operativos")%>%select(Codigo,Final)%>%arrange(desc(Final))%>%slice(1:5)
  #18.Alumnos top5 alumnos ArquiComp
  DPLYR18<-ICSI%>%filter(Curso=="Arquitectura de computadoras")%>%select(Codigo,Final)%>%arrange(desc(Final))%>%slice(1:5)
  #19.Join entre Sisope y ArquiComp
  DPLYR19<-full_join(DPLYR17,DPLYR18,by="Codigo")
  #20.Cantidad entre aprobados de MPN1 y MPN2
  DPLYR20.1<-ICSI %>% filter(Final>=11,Curso=='Modelado de Proceso de Negocios 1') %>% summarise(n()) %>% mutate(curso="MPN1")
  DPLYR20.2<-ICSI %>% filter(Final>=11,Curso=='Modelado de Proceso de Negocios 2') %>% summarise(n()) %>% mutate(curso="MPN2")
  DPLYR20<-bind_rows(DPLYR20.1,DPLYR20.2)
  
##DATA.TABLE
  #Consultas
  ICSIdt<-as.data.table(ICSI)
  rawICSIdt<-as.data.table(rawICSI)
  #1.Cantidad desaprobados en 4ciclo
  DT1<-as.data.table(ICSIdt[Ciclo==3 & Final<11, .N])%>%mutate(ciclo="4ciclo")
  
  #2.Cantidad inhabilitado en 1ciclo
  DT2<-as.data.table(rawICSIdt[Ciclo==1 & Final=="IN", .N])
  
  #3.Cantidad aprobados en 6ciclo
  DT3<-as.data.table(ICSIdt[Ciclo==6 & Final>10, .N])%>%mutate(ciclo="6ciclo")
  
  #4.Promedio de notas en 5ciclo
  DT4<-as.data.table(ICSIdt[Ciclo==5, .(mean(Final))])
  
  #5.Cantidad de alumnos por segunda en el curso de MPN2
  DT5<-as.data.table(ICSIdt[Curso=="Modelado de Proceso de Negocios 2" & Vez==2, .N])
  
  #6.Cantidad de alumnos por segunda en el 1ciclo
  DT6<-as.data.table(ICSIdt[Ciclo==1 & Vez==2, .N])
  
  #7.Numero de alumnos con <=10 del curso ProyectoInvestigacion
  DT7<-as.data.table(ICSIdt[Final<=10 & Curso=="Proyecto de Investigacion", .N])%>%mutate(ciclo="Des")
  
  #8.Numero de alumnos con 10<x=<15 del curso ProyectoInvestigacion
  DT8<-as.data.table(ICSIdt[Final>10 & Final<=15 & Curso=="Proyecto de Investigacion", .N])%>%mutate(ciclo="Apro")
  
  #9.Numero de alumnos con >15 del curso ProyectoInvestigacion
  DT9<-as.data.table(ICSIdt[Final>15 & Curso=="Proyecto de Investigacion", .N])%>%mutate(ciclo="Mejores>15")
  
  #10.Relacion entre consultas 7,8 y 9
  DT10<-bind_rows(DT7,DT8,DT9)
  
  #11.Numero de inhabilitados de cada curso en 2 ciclo
  DT11.1<-as.data.table(rawICSIdt[Curso=="Programcion Orienta a Objetos" & Final=="IN", .N])%>%mutate(ciclo="POO")
  DT11.2<-as.data.table(rawICSIdt[Curso=="Calculo" & Final=="IN", .N])%>%mutate(ciclo="Calculo")
  DT11.3<-as.data.table(rawICSIdt[Curso=="Matematica Discreta" & Final=="IN", .N])%>%mutate(ciclo="MateDis")
  DT11.4<-as.data.table(rawICSIdt[Curso=="Automatas y Compiladores" & Final=="IN", .N])%>%mutate(ciclo="Automatas")
  DT11<-bind_rows(DT11.1,DT11.2,DT11.3,DT11.4)
  
  #12.Promedio de componentes del 1ciclo
  DT12<-as.data.table(ICSIdt[Ciclo==1, .(c1=mean(ICSIdt$C1),c2=mean(ICSIdt$C2),c3=mean(ICSIdt$C3),c4=mean(ICSIdt$C4))])
  
  #13.Promedio del Parcial del curso de ProyectoInvestigacion
  DT13<-as.data.table(ICSIdt[Curso=="Proyecto de Investigacion", .(promedio=mean(ICSIdt$EP))])
  
  #14.Cantidad de alumnos que no rindieron el examen final de ProyectInvestigacion
  DT14<-as.data.table(rawICSIdt[Curso=="Proyecto de Investigacion" & (Final=="IN" | Final<=10), .N])
  
  #15.¿Que promedio (final y parcial) del 8ciclo es mayor?
  DT15<-as.data.table(ICSIdt[Ciclo==1, .(final=mean(ICSIdt$EF),parcial=mean(ICSIdt$EP))])
  
  #16.Varianza de la nota promocional de 5ciclo
  DT16<-as.data.table(ICSIdt[Ciclo==5, .(var(ICSIdt$Prom))])
  
  #17.Alumnos que desaprobaron el parcial pero aprobaron el final en IHM
  DT17<-as.data.table(ICSIdt[Curso=="Interaccion Hombre Computador" & ICSIdt$EP<=10 & ICSIdt$EF>10])
  
  #18.Alumnos que aprobaron el parcial pero desaprobaron el final en IHM
  DT18<-as.data.table(ICSIdt[Curso=="Interaccion Hombre Computador" & ICSIdt$EP>10 & ICSIdt$EF<=10])
  
  #19.Promedio de nota final de los cursos de la linea de programacion
  DT19<-as.data.table(ICSIdt[Curso=="Introduccion a la Programacion" | Curso=="Programacion Orientada a Objetos" |
                               Curso=="Desarrollo de Aplicaciones", .(mean(ICSIdt$Final))])
  
  #20.Promedio de nota final de los cursos de la linea de SI
  DT20<-DT19<-as.data.table(ICSIdt[Curso=="Introduccion a la Ingenieria de Sistemas" |
                                     Curso=="Fundamentos de Sistemas de Informacion" |
                                     Curso=="Sistemas de Informacion", .(mean(ICSIdt$Final))])
  
  
  
##GGPLOT
  #1.Join alumnos desaprobados en POO e IntroProg (SQL6)
  GG1<-ggplot(SQL6,aes(x=SQL6$codIntro,y=SQL6$FinalIntro,fill=SQL6$codIntro))+ geom_bar(stat="identity")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  #2.Top5 de Alumnos de 2ciclo con menos promedio promocional (SQL14)
  GG2<-ggplot(SQL14,aes(x=SQL14$Codigo,y=SQL14$Prom,fill=SQL14$Codigo))+geom_bar(stat="identity")+coord_flip()
  #3.Cantidad de inhabilitados y habilitados del curso de mateBasica (DPLYR1)
  GG3<-ggplot(DPLYR1,aes(x=DPLYR1$Tipo,y=DPLYR1$`n()`,fill=DPLYR1$Tipo))+geom_bar(stat="identity")
  #4.Cantidad de aprobados y desaprobados del 1°ciclo
  GG4<-ggplot(DPLYR2,aes(x=DPLYR2$Tipo,y=DPLYR2$`n()`,fill=DPLYR2$Tipo))+geom_bar(stat="identity")
  #5.Cantidad de aprobados de IntroProg y POO
  GG5<-ggplot(DPLYR3,aes(x=DPLYR3$Tipo,y=DPLYR3$`n()`,fill=DPLYR3$Tipo))+geom_bar(stat="identity")
  #6.Comparación de notas de la 1 mitad y 2 mitad del curso de mate3
  GG6<-ggplot(DPLYR8,aes(x=DPLYR8$Tipo,y=DPLYR8$Promedio,fill=DPLYR8$Tipo))+geom_bar(stat="identity")
  #7.Top 3 de alumnos del 3ciclo
  GG7<-ggplot(DPLYR11,aes(x=factor(1),y=DPLYR11$Prom,fill=DPLYR11$Codigo))+geom_bar(width=1,stat="identity")+ coord_polar("y")
  #8.top10 de alumnos IntroProg
  GG8<-ggplot(DPLYR14,aes(x=DPLYR14$Codigo,y=DPLYR14$Final,fill=DPLYR14$Final))+geom_point()+ggplot2::coord_flip()
  #9.top10 de alumnos POO
  GG9<-ggplot(DPLYR15,aes(x=DPLYR15$Codigo,y=DPLYR15$Final,fill=DPLYR15$Final))+geom_bar(stat="identity")+ggplot2::coord_flip()
  #10.Alumnos top5 alumnos Sisope
  GG10<-ggplot(DPLYR17,aes(x=DPLYR17$Codigo,y=DPLYR17$Final,fill=DPLYR17$Final))+geom_bar(stat="identity")
  #11.Alumnos top5 alumnos ArquiComp
  GG11<-ggplot(DPLYR18,aes(x=DPLYR18$Codigo,y=DPLYR18$Final,fill=DPLYR18$Final))+geom_bar(stat="identity")
  #12.Cantidad entre aprobados de MPN1 y MPN2
  GG12<-ggplot(DPLYR20,aes(x=factor(1),y=DPLYR20$`n()`,fill=DPLYR20$curso))+geom_bar(width=1,stat="identity")+coord_polar("y")
  #13.Cantidad de desaprobados en 4Ciclo vs cantidad de aprobados en 6Ciclo
  ggp13<-bind_rows(DT1,DT3)
  GG13<-ggplot(ggp13,aes(x=factor(1),y=ggp13$V1,fill=ggp13$ciclo))+geom_bar(width=1,stat="identity")+coord_polar("y")
  #14.Alumnos desaprobados, aprobados y con >15 en ProyectoInvestigacion
  GG14<-ggplot(DT10,aes(x=DT10$ciclo,y=DT10$V1,fill=DT10$V1))+geom_bar(stat="identity")
  #15.Aprobados y desaprobados en IntroIngSI
  ggp15.1<-ICSI%>%filter(Curso=="Introduccion a la Ingenieria de Sistemas",Final>10)%>%summarise(total=n())%>%mutate(Tipo="Apro")
  ggp15.2<-ICSI%>%filter(Curso=="Introduccion a la Ingenieria de Sistemas",Final<=10)%>%summarise(total=n())%>%mutate(Tipo="Desap")
  ggp15<-bind_rows(ggp15.1,ggp15.2)
  GG15<-ggplot(ggp15,aes(x=ggp15$Tipo,y=ggp15$total,fill=ggp15$Tipo))+geom_bar(stat="identity")
  #16.Cantidad de alumnos matriculados en 1ciclo vs 7ciclo
  ggp16.1<-ICSI%>%filter(Ciclo==1)%>%summarise(total=n())%>%mutate(Ciclo="1Ciclo")
  ggp16.2<-ICSI%>%filter(Ciclo==7)%>%summarise(total=n())%>%mutate(Ciclo="7Ciclo")
  ggp16<-bind_rows(ggp16.1,ggp16.2)
  GG16<-ggplot(ggp16,aes(x=factor(1),y=ggp16$total,fill=ggp16$Ciclo))+geom_bar(width=1,stat="identity")+coord_polar("y")
  #17.Aprobados y desaprobados en Calculo
  ggp17.1<-ICSI%>%filter(Curso=="Calculo",Final>10)%>%summarise(total=n())%>%mutate(Tipo="Apro")
  ggp17.2<-ICSI%>%filter(Curso=="Calculo",Final<=10)%>%summarise(total=n())%>%mutate(Tipo="Desap")
  ggp17<-bind_rows(ggp17.1,ggp17.2)
  GG17<-ggplot(ggp17,aes(x=ggp17$Tipo,y=ggp17$total,fill=ggp17$Tipo))+geom_bar(stat="identity")
  #18.Aprobados y desaprobados en BD
  ggp18.1<-ICSI%>%filter(Curso=="Base de datos",Final>10)%>%summarise(total=n())%>%mutate(Tipo="Apro")
  ggp18.2<-ICSI%>%filter(Curso=="Base de datos",Final<=10)%>%summarise(total=n())%>%mutate(Tipo="Desap")
  ggp18<-bind_rows(ggp18.1,ggp18.2)
  GG18<-ggplot(ggp15,aes(x=factor(1),y=ggp18$total,fill=ggp18$Tipo))+geom_bar(width=1,stat="identity")+coord_polar("y")
  #19.Desaprobados en AdmiRedes y ArquiRedes
  ggp19.1<-ICSI%>%filter(Curso=="Administracion de Redes y Seguridad de la Informacion",Final<=10)%>%summarise(total=n())%>%mutate(Tipo="AdmiR")
  ggp19.2<-ICSI%>%filter(Curso=="Arquitectura de Redes de Computadoras",Final<=10)%>%summarise(total=n())%>%mutate(Tipo="ArquiR")
  ggp19<-bind_rows(ggp19.1,ggp19.2)
  GG19<-ggplot(ggp19,aes(x=factor(1),y=ggp19$total,fill=ggp19$Tipo))+geom_bar(width=1,stat="identity")+coord_polar("y")
  #20.Aprobados en MateDiscreta y Mate3
  ggp20.1<-ICSI%>%filter(Curso=="Matematica Discreta",Final>10)%>%summarise(total=n())%>%mutate(Tipo="MateDiscreta")
  ggp20.2<-ICSI%>%filter(Curso=="Matematica III",Final>10)%>%summarise(total=n())%>%mutate(Tipo="Mate3")
  ggp20<-bind_rows(ggp20.1,ggp20.2)
  GG20<-ggplot(ggp20,aes(x=factor(1),y=ggp20$total,fill=ggp20$Tipo))+geom_bar(width=1,stat="identity")+coord_polar("y")
  
##PLOTLY
  #1.Notas del EP de alumnos del curso de fisica1
  plotly3<-plot_ly(DPLYR9,x=~Codigo,y=~EP,type="scatter")
  #2.Notas del Final de alumnos del curso de fisica1
  plotly2<-plot_ly(DPLYR9,x=~Codigo,y=~Final,type="box")
  #3.Notas de EP y Final de alumnos de Fisica1
  plotly1<-plot_ly(DPLYR9,x=~Codigo,y=~EP,type="scatter",name="ExamParcial") %>%
    add_trace(y=~Final,type="bar",name="NotaFinal")
  #4.Join entre IntroProg y POO
  plotly4<-plot_ly(DPLYR16,x=~Final.x,y=~Codigo,type="bar",name="NotaIntroProg") %>%
    add_trace(x=~Final.y,type="scatter",name="POO")
  #5.Join entre Sisope y ArquiComp
  plotly5<-plot_ly(DPLYR19,x=~Final.x,y=~Codigo,type="bar",name="Sisope") %>%
    add_trace(x=~Final.y,type="scatter",name="ArquiComp")
  #6.Notas de alumnos de IntroIngSI
  p6<-ICSI%>%filter(Curso=="Introduccion a la Ingenieria de Sistemas")
  plotly6<-plot_ly(p6,x=~Codigo,y=~Final,type="box")
  #7.Notas de alumnos de MPN2
  p7<-ICSI%>%filter(Curso=="Modelado de Proceso de Negocios 2")
  plotly7<-plot_ly(p7,x=~Codigo,y=~Final,type="scatter")
  #8.Notas de Peti y geti
  p8<-ICSI%>%filter(Curso=="Planeamiento Estrategico de TI" | Curso=="Gestion de TI")
  plotly8<-plot_ly(p8,x=~Codigo,y=~Final,color=~Curso,type="bar")
  #9.Notas de los componente 1 y 4 de Sistema de Soporte de Decision
  p9<-ICSI%>%filter(Curso=="Sistema de Soporte de Decision")
  plotly9<-plot_ly(p9,x=~Codigo,y=~C1,type="bar",name="C1") %>% 
    add_trace(y=~C4,type="scatter",name="C4")
  #10.Notas de los componente 2 y 3 de Sistema de Soporte de Decision
  plotly10<-plot_ly(p9,x=~Codigo,y=~C2,type="bar",name="C2") %>%
    add_trace(y=~C3,type="scatter",name="C3")
  #11.Relacion de aprobado y desaprobados en el EP de Investigacion de Operaciones
  p11.1<-ICSI%>%filter(Curso=="Investigacion de Operaciones",EP>=11)%>%mutate(Tipo="Aprob")
  p11.2<-ICSI%>%filter(Curso=="Investigacion de Operaciones",EP<11)%>%mutate(Tipo="Desap")
  p11<-bind_rows(p11.1,p11.2)
  plotly11<-plot_ly(p11,labels=~Tipo,values=~EP)%>% 
    add_pie(hole = 0.6) %>%
    layout(title = "Donut chart",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #12.Cantidad inhabilitados en 1ciclo vs 2ciclo
  p12.1<-rawICSI%>%filter(Final=="IN",Ciclo==1)%>%summarise(total=n())%>%mutate(Tipo="1Ciclo")
  p12.2<-rawICSI%>%filter(Final=="IN",Ciclo==2)%>%summarise(total=n())%>%mutate(Tipo="2Ciclo")
  p12<-bind_rows(p12.1,p12.2)
  plotly12<-plot_ly(p12,labels=~Tipo,values=~total)%>% 
    add_pie(hole = 0.6) %>%
    layout(title = "Donut chart",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #13.Cantidad de aprobados en 5ciclo vs 7ciclo
  p13.1<-ICSI%>%filter(Ciclo==5,EP>=11)%>%summarise(total=n())%>%mutate(Tipo="5Ciclo")
  p13.2<-ICSI%>%filter(Ciclo==7,EP>=11)%>%summarise(total=n())%>%mutate(Tipo="6Ciclo")
  p13<-bind_rows(p13.1,p13.2)
  plotly13<-plot_ly(p13,x=~Tipo,y=~total,type="bar")
  #14.Notas del examen final de Desarrollo de Aplicaciones
  p14<-ICSI%>%filter(Curso=="Desarrollo de Aplicaciones")
  plotly14<-plot_ly(p14,x=~Codigo,y=~EF,type="bar")
  #15.Notas del examen final de DAP y POO
  p15.1<-ICSI%>%filter(Curso=="Desarrollo de Aplicaciones")%>%summarise(promedio=mean(EF))%>%mutate(tipo="DAP")
  p15.2<-ICSI%>%filter(Curso=="Programacion Orientada a Objetos")%>%summarise(promedio=mean(EF))%>%mutate(tipo="POO")
  p15<-bind_rows(p15.1,p15.2)
  plotly15<-plot_ly(p15,labels=~tipo,values=~promedio)%>% 
    add_pie(hole = 0.6) %>%
    layout(title = "Donut chart",  showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #16.Notas promocional del curso INSOII
  p16<-ICSI%>%filter(Curso=="Ingenieria de Software 2")
  plotly16<-plot_ly(p16,x=~Codigo,y=~Prom,type="scatter")
  #17.Notas ExamnFinal y final de MPN2
  p17<-ICSI%>%filter(Curso=="Modelado de Proceso de Negocios 2")
  plotly17<-plot_ly(p17,x=~Codigo,y=~EF,type="scatter",name="ExaFinal") %>% add_trace(y=~Final,type="bar",name="Final")
  #18.Notas del parcial de Proyecto de Investigación
  p18<-ICSI%>%filter(Curso=="Proyecto de Investigacion")
  plotly18<-plot_ly(p17,x=~Codigo,y=~EP,type="scatter")
  #19.Notas del C3 de Proyecto de Investigación
  plotly19<-plot_ly(p17,x=~Codigo,y=~C3,type="bar")
  #20.Notas del C4 de Proyecto de Investigación
  plotly20<-plot_ly(p17,x=~Codigo,y=~C4,type="scatter")

  ## Modelo Regresión Lineal
  MAlum<-function(ds,cr1,cr2,newX){
    ds1<-ds%>%filter(Curso==cr1)
    ds2<-ds%>%filter(Curso==cr2)
    m1M<-inner_join(ds1,ds2,by="Codigo")
    x<-m1M$Final.x
    y<-m1M$Final.y
    m2M<-data.frame(x,y)
    xy<-x*y
    x2<-x^2
    y2<-y^2
    lista<-list()
    #Generando un nuevo dataframe
    lista[[1]]<-data.frame(x,y,xy,x2,y2)
    px<-mean(x)
    py<-mean(y)
    pxy<-mean(xy)
    px2<-mean(x2)
    py2<-mean(y2)
    cov<-pxy-(px*py)
    sdx<-sqrt(px2-(px^2))
    sdy<-sqrt(py2-(py^2))
    pearson<-cov/(sdx*sdy)
    varX<-sdx^2
    #cambie y
    y<-(cov/varX)*(x-px)+py
    m3M<-data.frame(x,y)
    newY<-(cov/varX)*(newX-px)+py
    lista[[2]]=cov
    lista[[3]]=pearson
    lista[[4]]=newY
    lista[[5]]=m2M
    lista[[6]]=m3M
    return(lista)
  }
  M1mf<-MAlum(ICSI,"Introduccion a la Programacion","Programacion Orientada a Objetos",c(14,16,17))
  M1mf[[3]]
  M1mf[[4]]
  ggplot()+
    geom_point(data=M1mf[[5]],aes(x=x,y=y))+
    geom_line(data=M1mf[[6]],aes(x=x,y=y))
  
  M2mf<-MAlum(ICSI,"Matematica Basica","Matematica Discreta",c(14,16,17))
  M2mf[[3]]
  ggplot()+
    geom_point(data=M2mf[[5]],aes(x=x,y=y))+
    geom_line(data=M2mf[[6]],aes(x=x,y=y))
  
  M3mf<-MAlum(ICSI,"Matematica Basica","Calculo",c(14,16,17))
  M3mf[[3]]
  ggplot()+
    geom_point(data=M3mf[[5]],aes(x=x,y=y))+
    geom_line(data=M3mf[[6]],aes(x=x,y=y))
  
  M4mf<-MAlum(ICSI,"Matematica Discreta","Matematica III",c(14,16,17))
  M4mf[[3]]
  ggplot()+
    geom_point(data=M4mf[[5]],aes(x=x,y=y))+
    geom_line(data=M4mf[[6]],aes(x=x,y=y))
  
  M5mf<-MAlum(ICSI,"Introduccion a la Ingenieria de Sistemas","Fundamentos de Sistemas de Informacion",c(14,16,17))
  M5mf[[3]]
  ggplot()+
    geom_point(data=M5mf[[5]],aes(x=x,y=y))+
    geom_line(data=M5mf[[6]],aes(x=x,y=y))
  
  M6mf<-MAlum(ICSI,"Fisica 1","Fisica aplicada a la computacion",c(14,16,17))
  M6mf[[3]]
  ggplot()+
    geom_point(data=M6mf[[5]],aes(x=x,y=y))+
    geom_line(data=M6mf[[6]],aes(x=x,y=y))
  
    M7mf<-MAlum(ICSI,"Estructura de datos","Base de datos",c(14,16,17))
  M7mf[[3]]
  ggplot()+
    geom_point(data=M7mf[[5]],aes(x=x,y=y))+
    geom_line(data=M7mf[[6]],aes(x=x,y=y))
  
  M8mf<-MAlum(ICSI,"Programacion Orientada a Objetos","Desarrollo de Aplicaciones",c(14,16,17))
  M8mf[[3]]
  ggplot()+
    geom_point(data=M8mf[[5]],aes(x=x,y=y))+
    geom_line(data=M8mf[[6]],aes(x=x,y=y))
  
  M9mf<-MAlum(ICSI,"Ingenieria de Software 1","Ingenieria de Software 2",c(14,16,17))
  M9mf[[3]]
  ggplot()+
    geom_point(data=M9mf[[5]],aes(x=x,y=y))+
    geom_line(data=M9mf[[6]],aes(x=x,y=y))
  
  M10mf<-MAlum(ICSI,"Base de datos","Sistema Gestor de Base de Datos",c(14,16,17))
  M10mf[[3]]
  ggplot()+
    geom_point(data=M10mf[[5]],aes(x=x,y=y))+
    geom_line(data=M10mf[[6]],aes(x=x,y=y))
  
  M11mf<-MAlum(ICSI,"Modelado de Proceso de Negocios 1","Modelado de Proceso de Negocios 2",c(14,16,17))
  M11mf[[3]]
  ggplot()+
    geom_point(data=M11mf[[5]],aes(x=x,y=y))+
    geom_line(data=M11mf[[6]],aes(x=x,y=y))
  
  M12mf<-MAlum(ICSI,"Arquitectura de computadoras","Sistemas Operativos",c(14,16,17))
  M12mf[[3]]
  ggplot()+
    geom_point(data=M12mf[[5]],aes(x=x,y=y))+
    geom_line(data=M12mf[[6]],aes(x=x,y=y))
  
  M13mf<-MAlum(ICSI,"Automatas y Compiladores","Estructura de datos",c(14,16,17))
  M13mf[[3]]
  ggplot()+
    geom_point(data=M13mf[[5]],aes(x=x,y=y))+
    geom_line(data=M13mf[[6]],aes(x=x,y=y))
  
  M14mf<-MAlum(ICSI,"Sistemas Operativos","Arquitectura de Redes de Computadoras",c(14,16,17))
  M14mf[[3]]
  ggplot()+
    geom_point(data=M14mf[[5]],aes(x=x,y=y))+
    geom_line(data=M14mf[[6]],aes(x=x,y=y))
  
  M15mf<-MAlum(ICSI,"Fundamentos de Sistemas de Informacion","Sistemas de Informacion",c(14,16,17))
  M15mf[[3]]
  ggplot()+
    geom_point(data=M15mf[[5]],aes(x=x,y=y))+
    geom_line(data=M15mf[[6]],aes(x=x,y=y))
  
  M16mf<-MAlum(ICSI,"Arquitectura de Redes de Computadoras","Administracion de Redes y Seguridad de la Informacion",c(14,16,17))
  M16mf[[3]]
  ggplot()+
    geom_point(data=M16mf[[5]],aes(x=x,y=y))+
    geom_line(data=M16mf[[6]],aes(x=x,y=y))
  
  M17mf<-MAlum(ICSI,"Sistema Gestor de Base de Datos","Sistema de Soporte de Decision",c(14,16,17))
  M17mf[[3]]
  ggplot()+
    geom_point(data=M17mf[[5]],aes(x=x,y=y))+
    geom_line(data=M17mf[[6]],aes(x=x,y=y))
  
  M18mf<-MAlum(ICSI,"Planeamiento Estrategico de TI","Gestion de TI",c(14,16,17))
  M18mf[[3]]
  ggplot()+
    geom_point(data=M18mf[[5]],aes(x=x,y=y))+
    geom_line(data=M18mf[[6]],aes(x=x,y=y))
  
  M19mf<-MAlum(ICSI,"Sistemas de Informacion","Administacion de Proyectos de Sistemas de Informacion",c(14,16,17))
  M19mf[[3]]
  ggplot()+
    geom_point(data=M19mf[[5]],aes(x=x,y=y))+
    geom_line(data=M19mf[[6]],aes(x=x,y=y))

