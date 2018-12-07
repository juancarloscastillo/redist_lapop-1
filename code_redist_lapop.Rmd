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
#Eliminamos casos duplicados generados
lpop <- ddply(lpop, .(lpop$pais, lpop$year, lpop$id), unique)
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
dim(lpop$confianza)
```

+ Variable ingreso

```{r eval=FALSE, message=FALSE, warning=FALSE, echo=TRUE} 
#Ingreso 
#Mexico: cod. 1
#2008
freq(to_label(lpop$ingreso[lpop$year==2008 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=400;2=1200;3=2000;4=2800;5=3600;6=4700;7=6100;8=8400;9=11750;10=13500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2008 & lpop$pais=="MEX"])

#2010
freq(to_label(lpop$ingreso[lpop$year==2010 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=400;2=1200;3=2000;4=2800;5=3600;6=4700;7=6100;8=8400;9=11750;10=13500;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2010 & lpop$pais=="MEX"])

#2012
freq(to_label(lpop$ingreso[lpop$year==2012 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=240;2=600;3=840;4=1200;5=1795;6=2510;7=3230;8=3950;9=5385;10=6820; 11=7895;12=9330;13=10770;14=12205;15=13640;16=14360;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2012 & lpop$pais=="MEX"])

#2014
freq(to_label(lpop$ingreso[lpop$year==2014 & lpop$pais=="MEX"]))
lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"]<- rec(lpop$ingreso,rec="1=350;2=875;3=1275;4=1675;5=2025;6=2475;7=3000;8=3475;9=3900;10=4275; 11=4925;12=5925;13=6850;14=8000;15=9950;16=11150;88=NA;98=NA")
table(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"])
summary(lpop$ingreso_r[lpop$year==2014 & lpop$pais=="MEX"])

#Guatemala: cod. 2
#2008

#2010

#2012

#2014


#Salvador: cod. 3
#2008

#2010

#2012

#2014

#Honduras: cod. 4
#2008

#2010

#2012

#2014

#Nicaragua: cod. 5
#2008

#2010

#2012

#2014

#Costa Rica: cod. 6
#2008

#2010

#2012

#2014

#Panama: cod. 7
#2008

#2010

#2012

#2014

#Colombia: cod. 8
#2008

#2010

#2012

#2014

#Ecuador: cod. 9
#2008

#2010

#2012

#2014

#Bolivia: cod. 10
#2008

#2010

#2012

#2014

#Peru: cod. 11
#2008

#2010

#2012

#2014

#Paraguay: cod. 12
#2008

#2010

#2012

#2014

#Chile: cod. 13
#2008

#2010

#2012

#2014

#Uruguay: cod. 14
#2008

#2010

#2012

#2014

#Brasil: cod. 15
#2008

#2010

#2012

#2014

#Venezuela: cod. 16
#2008

#2010

#2012

#2014

#Argentina: cod. 17
#2008

#2010

#2012

#2014

#República Dominicana: cod. 18
#2008

#2010

#2012

#2014"

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
load("lpop.RData")
describe(lpop)
```



