---
title: 'Documento de código: Preferencias redistributivas en contextos desiguales'
author: "Gonzalo Franetovic"
date: "22/10/2018"
output:
  html_document: default
  word_document: default
---
 A continuación se presenta el documento de código de la presente investigación. Se expone paso a paso el ajuste y fundición de bases de datos, limpieza y recodificación de variables, estimación de modelo y visualizaciones gráficas. Cabe precisar que se extrajeron las bases de datos directamente desde la página web de Lapop, por lo que hay diferencias mínimas en algunas variables. 
 
+ Instalación Paquetes de trabajo

```{r echo=TRUE, message=FALSE, warning=FALSE}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, stargazer, haven, foreign, stargazer, xtable, psych, psy, base, magrittr, readxl, lme4, texreg, varhandle, countrycode, laeken, multilevel, ggplot2, ggthemes, extrafont, plyr, data.table, doBy, multiwayvcov, miceadds, RColorBrewer, scales, haven, margins, grDevices, interplot, lmtest, lsmeans, lattice, broom, sjPlot, lmerTest, labelled, dplyr, sjmisc, tidyverse, Hmisc, devtools, DescTools, descr)
search()


```

+ Lectura de bases de datos LAPOP 2008, 2010, 2012, 2014 y transformación a dataframe.

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Directorio y bases de datos
lp08 <- read_dta("2008.dta")
lp10 <- read_dta("2010.dta")
lp12 <- read_dta("2012.dta")
lp14 <- read_dta("2014.dta")
#Transformación a Data Frame 
lp08=as.data.frame(lp08) 
lp10=as.data.frame(lp10)
lp12=as.data.frame(lp12)
lp14=as.data.frame(lp14)
#Nombres de las variables en minúscula
names(lp08)= tolower(names(lp08)) 
names(lp10)= tolower(names(lp10)) 
names(lp12)= tolower(names(lp12)) 
names(lp14)= tolower(names(lp14)) 
```

+ Exploración y fundición de la bases de datos 

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Revisamos los primeros casos
head (lp08)
head (lp10)
head (lp12)
head (lp14)
#Generamos variable year para la base de 2014
lp14$year <- "2014"
#Selección de variables
#2008
lp081=lp08 %>% dplyr::select(pais, year, idnum, weight1500, ros4, q1, q2, q11, l1, ocup4a, ed, q10, ur, tamano, b12, b18, b10a, b21a, b13, b21, q12)
colnames(lp081) <- c("pais","year", "id", "weight1500", "redistribucion", "sexo", "edad", "ecivil", "ideopolitica", "sitlaboral", "educacion", "ingreso", "zona", "tamanociudad", "conffaa", "confpolicia", "confjudicial", "confejecutivo", "confcongreso", "confpartidos", "hijos")
head (lp081)
#2010
lp101=lp10 %>% dplyr::select(pais, year, idnum, weight1500, ros4, q1, q2, q11, l1, ocup4a, ed, q10, ur, tamano, b12, b18, b10a, b21a, b13, b21, q12)
colnames(lp101) <- c("pais","year", "id", "weight1500" , "redistribucion", "sexo", "edad", "ecivil", "ideopolitica", "sitlaboral", "educacion", "ingreso", "zona", "tamanociudad", "conffaa", "confpolicia", "confjudicial", "confejecutivo", "confcongreso", "confpartidos", "hijos")
head (lp101)
#2012
lp121=lp12 %>% dplyr::select(pais, year, idnum, weight1500, ros4, q1, q2, q11, l1, ocup4a, ed, q10new, ur, tamano, b12, b18, b10a, b21a, b13, b21, q12)
colnames(lp121) <- c("pais","year", "id", "weight1500" , "redistribucion", "sexo", "edad", "ecivil", "ideopolitica", "sitlaboral", "educacion", "ingreso", "zona", "tamanociudad", "conffaa", "confpolicia", "confjudicial", "confejecutivo", "confcongreso", "confpartidos", "hijos")
head (lp121)
#2014
lp141=lp14 %>% dplyr::select(pais, year, uniq_id, weight1500, ros4, q1, q2, q11n, l1, ocup4a, ed, q10new, ur, tamano, b12, b18, b10a, b21a, b13, b21, q12)
colnames(lp141) <- c("pais","year", "id", "weight1500", "redistribucion", "sexo", "edad", "ecivil", "ideopolitica", "sitlaboral", "educacion", "ingreso", "zona", "tamanociudad", "conffaa", "confpolicia", "confjudicial", "confejecutivo", "confcongreso", "confpartidos", "hijos")
head (lp141)
 #Fundir Bases de datos
lpop=rbind(lp081, lp101, lp121, lp141)
lpop <- lpop[order(lpop$pais), ]
```
 
+ Selección e identificación de los países de la muestra
 
```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Removemos los países que no vamos a considerar en la muestra
lpop<-lpop[!(lpop$pais>21),]
lpop$pais[lpop$pais==21] <- "18"
#Países de la muestra (Etiquetas)
lpop$pais <- factor(lpop$pais,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
labels = c("MEX", "GTM", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "ECU", "BOL", "PER", "PRY", "CHL", "URY", "BRA", "VEN", "ARG", "DOM"))
describe(lpop$pais)
describe(lpop$year)
```
 
+ Imputamos variable "WT" (Peso País) desde base externa para posterior ponderación de las bases de datos individuales de los países
 
```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Importamos base de datos externa
wt <- read_dta("wt.dta")
wt=as.data.frame(wt) #Transformamos a data frame
colnames(wt) <- c("pais", "year", "id", "wt")
#Removemos los países y olas que no vamos a considerar en la muestra
wt<-wt[!(wt$pais>21),]
wt$pais[wt$pais==21] <- "18"
wt$pais <- factor(wt$pais,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
labels = c("MEX", "GTM", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "ECU", "BOL", "PER", "PRY", "CHL", "URY", "BRA", "VEN", "ARG", "DOM"))
wt<-wt[!(wt$year<2008),]
wt <- wt[order(wt$pais), ]
#Merge: Se agregan observaciones con "wt", pero sin información para las variabales del estudio: Se procede a borrar esos casos
lpop <- merge(lpop, wt, by=c("pais", "year", "id"), all.x= TRUE)
#Eliminamos casos duplicados generados y variables merge
lpop <- ddply(lpop, .(lpop$pais, lpop$year, lpop$id), unique)
lpop$`lpop$pais` <-NULL
lpop$`lpop$year` <-NULL
lpop$`lpop$id` <-NULL
#Revisamos
table(lpop$pais, lpop$year)
```
 
+ Variables de nivel individual

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Olas: 
lpop$wave <- lpop$year
lpop$wave=rec(lpop$wave, rec=c("2008=08", "2010=10", "2012=12", "2014=14"))
describe(lpop$wave)

#Pais_Año:
lpop$pais_wave <- do.call(paste, c(lpop[c("pais", "wave")], sep = "_")) 

#Redistribución:
# Dummy
lpop$redistribucion_r <-as.numeric(lpop$redistribucion >= 7)
as.factor(lpop$redistribucion_r)

# Log Natural
lpop$redistribucion_ln <- log(lpop$redistribucion)
#Tablas
table(lpop$redistribucion)
table(lpop$redistribucion_r)
table(lpop$redistribucion_ln)
with(lpop, table(lpop$pais, lpop$year))

#Hombre
lpop$sexo <-as.numeric(lpop$sexo <= 1)
lpop$sexo <-factor(lpop$sexo, levels = c(0,1), labels = c("Mujer", "Hombre"))
table(lpop$sexo)

#Edad
# Continua
lpop$edad[lpop$edad<=17] <- NA
# Cuadrática 
lpop$edad2 <- (lpop$edad)^2
#Tablas
table(lpop$edad)
table(lpop$edad2)

#Casado (o conviviente)
lpop$ecivil_r=rec(lpop$ecivil, rec="2=1; 3=1; 7=1; 1=0; 4:6=0")
lpop$ecivil_r <-factor(lpop$ecivil_r, levels = c(0,1), labels = c("Soltero", "Casado"))
table(lpop$ecivil)
table(lpop$ecivil_r)

#Izquierda
#Continua
lpop$izquierda <- 1+10-(lpop$ideopolitica)
describe(lpop$ideopolitica)
describe(lpop$izquierda)
#Factor
lpop$ideopolitica_f=rec(lpop$izquierda, rec="1:4=1; 5:6=2; 7:10=3")
lpop$ideopolitica_f <-factor(lpop$ideopolitica_f, levels = c(1,2,3), labels = c("Derecha", "Centro", "Izquierda"))
describe(lpop$ideopolitica_f)

#Situación Laboral
lpop$sitlaboral_r=rec(lpop$sitlaboral, rec="4=1; 6=1; 3=2; 5=2; 7=2; 1:2=3")
lpop$sitlaboral_r <-factor(lpop$sitlaboral_r, levels = c(1,2,3), labels = c("No fuerza laboral", "Desempleado", "Empleado"))
describe(lpop$sitlaboral_r)

#Educación 
lpop$educacion_r=rec(lpop$educacion, rec="0:6=1; 7:12=2; 13:18=3")
lpop$educacion_r <-factor(lpop$educacion_r, levels = c(1,2,3), labels = c("Primaria", "Secundaria", "Terciaria"))
describe(lpop$educacion_r)

#Urbano
lpop$zona[lpop$zona==2] <- 0
lpop$zona <-factor(lpop$zona, levels = c(0,1), labels = c("Rural", "Urbano"))
table(lpop$zona)

#Tamaño Ciudad
lpop$tamano <- lpop$tamanociudad
lpop$tamanociudad <- 1+5-(lpop$tamano)
lpop$tamanociudad[lpop$tamanociudad<1] <- NA
lpop$tamanociudad <-factor(lpop$tamanociudad, levels = c(1,2,3,4,5), labels = c("Área rural", "Ciudad pequeña", "Ciudad mediana", "Ciudad grande", "Capital nacional"))
describe(lpop$tamanociudad)

#Promedio confianza
lpop$confianza <- rowMeans(lpop[c("conffaa", "confpolicia", "confjudicial", "confejecutivo", "confcongreso", "confpartidos")], na.rm=TRUE)
describe(lpop$confianza)

#Personas en el hogar: se toma la cantidad de hijos en el hogar como un proxy de la variable con un ajuste de +1
lpop$nhogar <- lpop$hijos+1
describe(lpop$hijos)
describe(lpop$nhogar)
```

+ Variable ingreso

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Ingreso: Se imputa el punto medio del rango para cada atributo de la respuesta, según la escala de cada país (tipo de moneda)

#Mexico: cod. 1
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=400;2=1201;3=2001;4=2801;5=3601;6=4701;7=6101;8=8401;9=11751;10=13500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=400;2=1201;3=2001;4=2801;5=3601;6=4701;7=6101;8=8401;9=11751;10=13500;
88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=240;2=600;3=841;4=1201;5=1796;6=2511;7=3231;8=3951;9=5386;10=6821;11=7896;12=9331;13=10771;14=12206;15=13641;16=14360;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=350;2=875;3=1276;4=1676;5=2026;6=2476;7=3001;8=3476;9=3901;10=4276;11=4926;12=5926;13=6851;14=8001;15=9951;16=11150;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"])
#--------------------------------------------------------------------#
#Guatemala: cod. 2
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="GTM"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="GTM"]<- rec(lpop$ingreso,rec="1=500;2=1251;3=1751;4=2251;5=2901;6=3651;7=4501;8=5801;9=8051;10=9500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="GTM"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="GTM"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="GTM"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="GTM"]<- rec(lpop$ingreso,rec="1=500;2=1251;3=1751;4=2251;5=2901;6=3651;7=4501;8=5801;9=8051;10=9500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="GTM"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="GTM"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="GTM"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="GTM"]<- rec(lpop$ingreso,rec="1=180;2=540;3=901;4=1261;5=1621;6=1981;7=2346;8=2701;9=3056;10=3511;11=4051;12=4861;13=5941;14=7561;15=9726;16=10810;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="GTM"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="GTM"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="GTM"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="GTM"]<- rec(lpop$ingreso,rec="1=180;2=480;3=721;4=961;5=1201;6=1441;7=1681;8=1921;9=2161;10=2401;11=2696;12=3056;13=3511;14=4051;15=4861;16=5400;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="GTM"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="GTM"])

#--------------------------------------------------------------------#
#El salvador: cod. 3
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="SLV"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="SLV"]<- rec(lpop$ingreso,rec="1=23;2=68;3=118;4=217;5=361;6=505;7=649;8=865;9=1225;10=1441;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="SLV"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="SLV"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="SLV"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="SLV"]<- rec(lpop$ingreso,rec="1=23;2=68;3=118;4=217;5=361;6=505;7=649;8=865;9=1225;10=1441;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="SLV"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="SLV"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="SLV"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="SLV"]<- rec(lpop$ingreso,rec="1=15;2=40;3=56;4=76;5=116;6=161;7=226;8=316;9=406;10=496;11=586;12=721;13=946;14=1261;15=1621;16=1800;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="SLV"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="SLV"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="SLV"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="SLV"]<- rec(lpop$ingreso,rec="1=20;2=48;3=66;4=88;5=113;6=138;7=161;8=186;9=218;10=253;11=298;12=353;13=411;14=496;15=666;16=780;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="SLV"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="SLV"])

#--------------------------------------------------------------------#
#HONDURAS: cod. 4
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="HND"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="HND"]<- rec(lpop$ingreso,rec="1=238;2=713;3=1426;4=2376;5=3326;6=4751;7=6651;8=8551;9=11876;10=14251;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="HND"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="HND"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="HND"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="HND"]<- rec(lpop$ingreso,rec="1=500;2=1751;3=3501;4=5501;5=7501;6=10501;7=13501;8=16001;9=19001;10=20501;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="HND"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="HND"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="HND"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="HND"]<- rec(lpop$ingreso,rec="1=460;2=1375;3=2291;4=3211;5=4126;6=5041;7=6411;8=7786;9=8941;10=10316;11=11691;12=13756;13=16506;14=19256;15=22691;16=24750;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="HND"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="HND"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="HND"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="HND"]<- rec(lpop$ingreso,rec="1=1025;2=2475;3=3176;4=3725;5=4275;6=4826;7=5376;8=6051;9=6926;10=7801;11=8701;12=9826;13=10976;14=12201;15=14676;16=16450;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="HND"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="HND"])

#--------------------------------------------------------------------#
#Nicaragua: cod. 5
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="NIC"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="NIC"]<- rec(lpop$ingreso,rec="1=750;2=2251;3=3626;4=4876;5=7001;6=10626;7=14876;8=19126;9=23376;10=25500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="NIC"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="NIC"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="NIC"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="NIC"]<- rec(lpop$ingreso,rec="1=750;2=2251;3=3626;4=4876;5=7001;6=10626;7=14876;8=19126;9=23376;10=25500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="NIC"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="NIC"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="NIC"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="NIC"]<- rec(lpop$ingreso,rec="1=335;2=835;3=1171;4=1671;5=2336;6=2841;7=3181;8=3681;9=4511;10=5511;11=7016;12=10026;13=14036;14=18045;15=22055;16=24060;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="NIC"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="NIC"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="NIC"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="NIC"]<- rec(lpop$ingreso,rec="1=550;2=1375;3=1801;4=2251;5=2776;6=3101;7=3351;8=3726;9=4201;10=4776;11=5426;12=6051;13=7026;14=8751;15=11651;16=13500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="NIC"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="NIC"])

#--------------------------------------------------------------------#
#Costa Rica: cod. 6
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="CRI"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CRI"]<- rec(lpop$ingreso,rec="1=5000;2=55000;3=117500;4=155000;5=197500;6=245000;7=310000;8=400000;9=575000;10=700000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CRI"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CRI"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="CRI"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CRI"]<- rec(lpop$ingreso,rec="1=53500;2=141528;3=203103;4=264869;5=335294;6=422001;7=539601;8=719638;9=1029788;10=1226501;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CRI"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CRI"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="CRI"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CRI"]<- rec(lpop$ingreso,rec="1=32060;2=80146;3=112201;4=160291;5=224086;6=272171;7=304871;8=352956;9=432776;10=528951;11=625126;12=721296;13=817466;14=961726;15=1154071;16=1250241;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CRI"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CRI"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="CRI"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CRI"]<- rec(lpop$ingreso,rec="1=44300;2=103151;3=134551;4=172976;5=209776;6=239326;7=266876;8=292626;9=318576;10=357176;11=417926;12=494551;13=581401;14=701901;15=917850;16=1059100;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CRI"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CRI"])

#--------------------------------------------------------------------#
#Panama: cod. 7
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="PAN"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PAN"]<- rec(lpop$ingreso,rec="1=50;2=150;3=300;4=500;5=700;6=900;7=1250;8=2000;9=3750;10=5000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PAN"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PAN"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="PAN"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PAN"]<- rec(lpop$ingreso,rec="1=50;2=150;3=300;4=500;5=700;6=900;7=1250;8=2000;9=3750;10=5000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PAN"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PAN"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="PAN"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PAN"]<- rec(lpop$ingreso,rec="1=60;2=145;3=201;4=291;5=406;6=491;7=551;8=636;9=781;10=956;11=1216;12=1736;13=2431;14=3126;15=3815;16=4160;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PAN"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PAN"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="PAN"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PAN"]<- rec(lpop$ingreso,rec="1=75;2=176;3=226;4=276;5=326;6=376;7=421;8=456;9=491;10=526;11=571;12=641;13=736;14=856;15=1010;16=1100;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PAN"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PAN"])

#--------------------------------------------------------------------#
#Colombia: cod. 8
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="COL"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="COL"]<- rec(lpop$ingreso,rec="1=45500;2=136000;3=271000;4=541000;5=860500;6=1250001;7=1750001;8=2500001;9=3500001;10=4000001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="COL"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="COL"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="COL"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="COL"]<- rec(lpop$ingreso,rec="1=45500;2=136000;3=271000;4=541000;5=860500;6=1250001;7=1750001;8=2500001;9=3500001;10=4000001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="COL"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="COL"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="COL"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="COL"]<- rec(lpop$ingreso,rec="1=45000;2=135000;3=225000;4=315000;5=405000;6=495000;7=585000;8=670000;9=755000;10=870000;11=1020000;12=1350000;13=1850000;14=2650000;15=3750000;16=4300000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="COL"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="COL"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="COL"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="COL"]<- rec(lpop$ingreso,rec="1=80000;2=205000;3=295001;4=380001;5=450001;6=510001;7=565001;8=620001;9=685001;10=765001;11=885001;12=1030001;13=1250001;14=1650001;15=2550000;16=3200000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="COL"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="COL"])

#--------------------------------------------------------------------#
#Ecuador: cod. 9
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="ECU"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ECU"]<- rec(lpop$ingreso,rec="1=30;2=81;3=151;4=251;5=401;6=626;7=876;8=1251;9=1751;10=2001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ECU"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ECU"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="ECU"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ECU"]<- rec(lpop$ingreso,rec="1=30;2=81;3=151;4=251;5=401;6=626;7=876;8=1251;9=1751;10=2001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ECU"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ECU"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="ECU"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="ECU"]<- rec(lpop$ingreso,rec="1=20;2=65;3=111;4=156;5=201;6=241;7=286;8=331;9=376;10=431;11=496;12=661;13=926;14=1321;15=1845;16=2110;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="ECU"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="GTM"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="ECU"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ECU"]<- rec(lpop$ingreso,rec="1=65;2=155;3=201;4=241;5=273;6=298;7=331;8=363;9=388;10=431;11=496;12=596;13=726;14=926;15=1320;16=1580;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ECU"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ECU"])

#--------------------------------------------------------------------#
#Bolivia: cod. 10
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="BOL"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BOL"]<- rec(lpop$ingreso,rec="1=125;2=375;3=650;4=1001;5=1601;6=2501;7=4001;8=7500;9=15000;10=20000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BOL"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BOL"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="BOL"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BOL"]<- rec(lpop$ingreso,rec="1=125;2=375;3=650;4=1001;5=1601;6=2501;7=4001;8=7500;9=15000;10=20000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BOL"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BOL"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="BOL"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BOL"]<- rec(lpop$ingreso,rec="1=70;2=205;3=341;4=476;5=611;6=751;7=886;8=1016;9=1151;10=1326;11=1531;12=2041;13=2856;14=4891;15=8150;16=9780;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BOL"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BOL"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="BOL"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BOL"]<- rec(lpop$ingreso,rec="1=125;2=375;3=651;4=951;5=1251;6=1551;7=1851;8=2201;9=2601;10=3051;11=3651;12=4401;13=5301;14=6401;15=8500;16=10000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BOL"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BOL"])

#--------------------------------------------------------------------#
#Peru: cod. 11
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="PER"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PER"]<- rec(lpop$ingreso,rec="1=50;2=151;3=301;4=501;5=701;6=1001;7=1401;8=1801;9=2500;10=3000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PER"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PER"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="PER"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PER"]<- rec(lpop$ingreso,rec="1=50;2=151;3=301;4=501;5=701;6=1001;7=1401;8=1801;9=2500;10=3000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PER"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PER"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="PER"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PER"]<- rec(lpop$ingreso,rec="1=55;2=170;3=286;4=396;5=506;6=621;7=736;8=846;9=956;10=1096;11=1266;12=1691;13=2366;14=3041;15=3715;16=4050;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PER"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PER"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="PER"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PER"]<- rec(lpop$ingreso,rec="1=105;2=270;3=381;4=481;5=571;6=656;7=741;8=821;9=896;10=961;11=1031;12=1141;13=1296;14=1616;15=2215;16=2580;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PER"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PER"])

#--------------------------------------------------------------------#
#Paraguay: cod. 12
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="PRY"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PRY"]<- rec(lpop$ingreso,rec="1=68500;2=206001;3=412501;4=687501;5=962501;6=1375001;7=1925001;8=2475001;9=3437501;10=4125001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PRY"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="PRY"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="PRY"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PRY"]<- rec(lpop$ingreso,rec="1=68500;2=206001;3=412501;4=687501;5=962501;6=1375001;7=1925001;8=2475001;9=3437501;10=4125001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PRY"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="PRY"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="PRY"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PRY"]<- rec(lpop$ingreso,rec="1=69095;2=207290;3=345486;4=483676;5=690966;6=967356;7=1243746;8=1520131;9=1931946;10=2346526;11=2628441;12=2902066;13=3175691;14=3731231;15=4560390;16=4974970;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PRY"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="PRY"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="PRY"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PRY"]<- rec(lpop$ingreso,rec="1=138195;2=414580;3=690966;4=967356;5=1220711;6=1451031;7=1681356;8=1911681;9=2142001;10=2372321;11=2628441;12=2902066;13=3175691;14=3731231;15=4560390;16=4974970;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PRY"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="PRY"])

#--------------------------------------------------------------------#
#Chile: cod. 13
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="CHL"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CHL"]<- rec(lpop$ingreso,rec="1=43000;2=122500;3=181500;4=241500;5=297000;6=374500;7=444000;8=492500;9=672000;10=813000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CHL"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="CHL"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="CHL"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CHL"]<- rec(lpop$ingreso,rec="1=43000;2=122500;3=181500;4=241500;5=297000;6=374500;7=444000;8=492500;9=672000;10=813000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CHL"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="CHL"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="CHL"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CHL"]<- rec(lpop$ingreso,rec="1=15165;2=45500;3=75836;4=106166;5=136501;6=166836;7=197471;8=227501;9=257531;10=295751;11=341251;12=409501;13=500501;14=637001;15=819000;16=910000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CHL"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="CHL"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="CHL"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CHL"]<- rec(lpop$ingreso,rec="1=61150;2=141300;3=172526;4=195276;5=218151;6=246526;7=277801;8=308676;9=344126;10=390626;11=446326;12=507876;13=591151;14=703901;15=850950;16=936000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CHL"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="CHL"])

#--------------------------------------------------------------------#
#Uruguay: cod. 14
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="URY"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="URY"]<- rec(lpop$ingreso,rec="1=2250;2=5251;3=7001;4=9001;5=11001;6=13001;7=16001;8=20501;9=28001;10=33001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="URY"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="URY"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="URY"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="URY"]<- rec(lpop$ingreso,rec="1=2250;2=5251;3=7001;4=9001;5=11001;6=13001;7=16001;8=20501;9=28001;10=33001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="URY"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="URY"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="URY"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="URY"]<- rec(lpop$ingreso,rec="1=1000;2=2500;3=3501;4=5001;5=6991;6=8491;7=9511;8=11011;9=13501;10=16501;11=19501;12=22501;13=25501;14=28501;15=31500;16=33000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="URY"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="URY"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="URY"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="URY"]<- rec(lpop$ingreso,rec="1=2450;2=6025;3=8076;4=9551;5=10801;6=12351;7=14226;8=16276;9=18301;10=20726;11=23701;12=27101;13=30101;14=32351;15=34550;16=35650;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="URY"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="URY"])

#--------------------------------------------------------------------#
#Brasil: cod. 15
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="BRA"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BREA"]<- rec(lpop$ingreso,rec="1=19000;2=57001;3=95001;4=152001;5=228001;6=285001;7=380001;8=513001;9=665001;10=760001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BRA"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="BRA"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="BRA"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BRA"]<- rec(lpop$ingreso,rec="1=25500;2=76501;3=127501;4=204001;5=306001;6=382501;7=510001;8=688501;9=892501;10=1020001;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BRA"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="BRA"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="BRA"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BRA"]<- rec(lpop$ingreso,rec="1=50;2=155;3=261;4=361;5=466;6=571;7=676;8=776;9=876;10=986;11=1086;12=1186;13=1321;14=1476;15=1705;16=1860;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BRA"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="BRA"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="BRA"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BRA"]<- rec(lpop$ingreso,rec="1=250;2=601;3=751;4=851;5=951;6=1051;7=1151;8=1301;9=1501;10=1701;11=1901;12=2551;13=3701;14=4851;15=6001;16=6601;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BRA"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="BRA"])

#--------------------------------------------------------------------#
#Venezuela: cod. 16
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="VEN"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="VEN"]<- rec(lpop$ingreso,rec="1=185;2=493;3=923;4=1536;5=2146;6=2761;7=3386;8=4001;9=4601;10=4901;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="VEN"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="VEN"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="VEN"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="VEN"]<- rec(lpop$ingreso,rec="1=185;2=493;3=923;4=1536;5=2146;6=2761;7=3386;8=4001;9=4601;10=4901;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="VEN"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="VEN"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="VEN"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="VEN"]<- rec(lpop$ingreso,rec="1=255;2=774;3=1293;4=1741;5=2128;6=2515;7=2902;8=3289;9=3676;10=4063;11=4450;12=4899;13=5418;14=5936;15=6447;16=6702;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="VEN"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="VEN"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="VEN"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="VEN"]<- rec(lpop$ingreso,rec="1=850;2=1901;3=2301;4=2626;5=2876;6=3176;7=3551;8=3951;9=4326;10=4651;11=5051;12=5651;13=6276;14=6976;15=8250;16=9100;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="VEN"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="VEN"])


#--------------------------------------------------------------------#
#Argentina: cod. 17
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="ARG"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ARG"]<- rec(lpop$ingreso,rec="1=250;2=751;3=1251;4=1751;5=2251;6=2751;7=3251;8=3751;9=4250;10=4500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ARG"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="ARG"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="ARG"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ARG"]<- rec(lpop$ingreso,rec="1=600;2=1601;3=2301;4=2951;5=3751;6=4701;7=6001;8=7851;9=10950;10=13000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ARG"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="ARG"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="ARG"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="ARG"]<- rec(lpop$ingreso,rec="1=385;2=960;3=1341;4=1916;5=2681;6=3256;7=3646;8=4221;9=5176;10=6326;11=7476;12=8626;13=9776;14=10926;15=12650;16=13800;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="ARG"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="ARG"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="ARG"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ARG"]<- rec(lpop$ingreso,rec="1=650;2=1500;3=1851;4=2201;5=2601;6=2951;7=3301;8=3651;9=3951;10=4301;11=4801;12=5451;13=6301;14=7401;15=8950;16=9900;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ARG"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="ARG"])


#--------------------------------------------------------------------#
#Republica Dominicana: cod. 18
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="DOM"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="DOM"]<- rec(lpop$ingreso,rec="1=438;2=1313;3=2626;4=4376;5=6126;6=8751;7=12251;8=15751;9=21876;10=38125;11=50000;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="DOM"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="DOM"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="DOM"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="DOM"]<- rec(lpop$ingreso,rec="1=1425;2=4288;3=6863;4=9151;5=11901;6=15001;7=22501;8=34501;9=50650;10=60800;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="DOM"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="DOM"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="DOM"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="DOM"]<- rec(lpop$ingreso,rec="1=505;2=1510;3=2516;4=3521;5=4526;6=5536;7=7036;8=8541;9=9806;10=11316;11=15091;12=21126;13=36211;14=54316;15=66385;16=72420;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="DOM"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="DOM"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="DOM"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="DOM"]<- rec(lpop$ingreso,rec="1=1400;2=3400;3=4601;4=5576;5=6451;6=7601;7=8801;8=10076;9=11251;10=12151;11=14026;12=16876;13=20201;14=25851;15=37875;16=46150;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="DOM"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="DOM"])

#Ingreso per cápita
lpop$ingreso_pc=lpop$ingreso_r/lpop$nhogar
lpop$ingreso_pc=log(lpop$ingreso_pc) #Logaritmo del ingreso per cápita

#Quintiles de ingreso
lpop %>% mutate(quintil=ntile(lpop$ingreso_pc,5)) -> lpop
freq(lpop$quintil)

#Deciles de ingreso
lpop %>% mutate(decil=ntile(lpop$ingreso_pc,10)) -> lpop
freq(lpop$decil)
```

+ Tiempo

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Dummy de tiempo
#2008
lpop$ano_2008 <- 0
lpop$ano_2008[lpop$year==2008] <- 1
#2008
lpop$ano_2010 <- 0
lpop$ano_2010[lpop$year==2010] <- 1
#2008
lpop$ano_2012 <- 0
lpop$ano_2012[lpop$year==2012] <- 1
#2008
lpop$ano_2014 <- 0
lpop$ano_2014[lpop$year==2014] <- 1
```

+ Variable de nivel 2

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Merge Variables de nivel país (2)
n2 <- read_dta("N2.dta")
n2=as.data.frame(n2) 
names(n2)= tolower(names(n2)) 
colnames(n2) <- c("pais", "year", "pais_nombre", "gini","pib", "gsocial", "desempleo", "informal", "bienestar", "bienestar1", "div_etnica", "div_religion", "div_lenguaje", "gini_mean", "pib_mean", "gsocial_mean", "desempleo_mean", "informal_mean", "gini_dif", "pib_dif", "gsocial_dif", "desempleo_dif", "informal_dif" )
#Países de la muestra (Etiquetas)
n2$pais <- factor(n2$pais,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
labels = c("MEX", "GTM", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "ECU", "BOL", "PER", "PRY", "CHL", "URY", "BRA", "VEN", "ARG", "DOM"))
#Año
n2$year <- as.factor(n2$year)
#Fundir
lpop <- merge(lpop, n2, by=c("pais", "year"))
#Corregir variable de nivel 2 como factor 
as.factor(lpop$redistribucion_r)
```

+ Base de datos transitorias

```{r echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE}
#Bases de datos intermedias
save(n2, file = "n2.RData")
save(wt, file = "wt.RData") #Variable wt
save(lp081, file = "lpop08.RData")
save(lp101, file = "lpop10.RData")
save(lp121, file = "lpop12.RData")
save(lp141, file = "lpop14.RData")
save(lpop, file = "lpop.RData") #Incluye variables de nivel 1 y 2
```

+ Descriptivos de las variables de nivel 1 y nivel 2

```{r echo=TRUE, message=FALSE, warning=FALSE}
lpop <- lpop[order(lpop$pais), ] #Ordenamos la base de datos
save(lpop, file = "lpop.RData") #Guardamos la base de datos
load("lpop.RData") #Cargamos la base
describe(lpop) #Estadísticos descriptivos de todas las variables 
```

