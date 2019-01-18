---
title: 'Code Document: Redistributive preferences in unequal contexts'
author: "Gonzalo Franetovic"
date: "18-11-2018"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

The research code document is detailed below. The adjustment of the databases, cleaning and recoding of variables, estimation of models, tables and graphic figures are explained step by step. Other analyzes available in the document "Code annex".
 
+ Install work packages

```{r Packages, message=FALSE, warning=FALSE, include=FALSE}
if (!require("pacman")) install.packages("pacman")
pacman::p_load(car, stargazer, haven, foreign, stargazer, xtable, psych, psy, base, magrittr, readxl, lme4, texreg, varhandle, countrycode, laeken, multilevel, ggplot2, ggthemes, extrafont, plyr, data.table, doBy, multiwayvcov, miceadds, RColorBrewer, scales, haven, margins, grDevices, interplot, lmtest, lsmeans, lattice, broom, sjPlot, lmerTest, labelled, dplyr, sjmisc, tidyverse, Hmisc, devtools, DescTools, descr)
search()
```

+ Full LAPOP database paid for by the Institute of Political Science of the Pontificia Universidad Católica de Chile (incorporates information omitted in the free version)

```{r Database Lapop full, echo=TRUE, message=FALSE, warning=FALSE}
#Directory and databases
lapop <- read_dta("Databases/OriginalLapop/Lapop_full_reduced.dta")
lapop=as.data.frame(lapop) #Transformation to Data Frame 
names(lapop)= tolower(names(lapop)) #Name of the variables in minuscule

#Name of the variables
lapop=lapop %>% dplyr::select(pais, year, idnum, weight1500, wt, ros4, q1, q2, q11, q11n, l1, ocup4a, ed, q10, q10new, ur, tamano, b12, b18, b10a, b21a, b13, b21, q12)
colnames(lapop) <- c("country","year", "id", "weight1500", "wt", "redistribution", "man", "age", "married1", "married2", "ideology", "employment", "education", "income1", "income2", "zone", "sizecity", "trustffaa", "trustpolice", "trustjudicial", "trustexecutive", "trustcongress", "trustpolparties", "children")

#Adjustment of different variables between years
#Married
lapop$married1[is.na(lapop$married1)] <- 99
lapop$married2[is.na(lapop$married2)] <- 99
lapop$married <- (lapop$married1+lapop$married2)-99
lapop$married1 <-NULL
lapop$married2 <-NULL
#Income
lapop$income1[is.na(lapop$income1)] <- 99
lapop$income2[is.na(lapop$income2)] <- 99
lapop$income <- (lapop$income1 + lapop$income2)-99
lapop$income[lapop$income==99] <- NA
lapop$income1 <-NULL
lapop$income2 <-NULL
```

+ Selection and identification of the countries and waves of the sample
 
```{r Sample of countries and waves, echo=TRUE, message=FALSE, warning=FALSE}
#Remove countries that we will not consider in the sample
lapop$country <- as.numeric(lapop$country)
lapop<-lapop[!(lapop$country>21),]
lapop$country[lapop$country==21] <- "18"
#Countries of the sample (Labels)
lapop$country <- factor(lapop$country,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
labels = c("MEX", "GTM", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "ECU", "BOL", "PER", "PRY", "CHL", "URY", "BRA", "VEN", "ARG", "DOM"))
#Remove waves that we will not consider in the sample
lapop<-lapop[!(lapop$year<2008),]
describe(lapop$country)
describe(lapop$year)
```
 
+ Individual level variables

```{r Variables Level 1, echo=TRUE, message=FALSE, warning=FALSE} 
#Waves: 
lapop$wave <- lapop$year
lapop$wave=rec(lapop$wave, rec=c("2008=08", "2010=10", "2012=12", "2014=14"))
lapop$year = as.factor(lapop$year)
#Country_year:
lapop$country_wave <- do.call(paste, c(lapop[c("country", "wave")], sep = "_")) 

#Redistribución:
#Dummy
lapop$redistribution_r <-as.numeric(lapop$redistribution >= 7)
lapop$redistribution_r <-as.factor(lapop$redistribution_r)
#Log Natural
lapop$redistribution_ln <- log(lapop$redistribution)

#Man
lapop$man <-as.numeric(lapop$man <= 1)
lapop$man <-factor(lapop$man, levels = c(0,1), labels = c("Woman", "Man"))

#Age
lapop$age[lapop$age<=17] <- NA #Continue Variable
lapop$age2 <- (lapop$age)^2 #Quadratic Variable

#Married (or cohabiting)
lapop$married <-as.factor(lapop$married)
lapop$married_f=rec(lapop$married, rec="2=1; 3=1; 7=1; 1=0; 4=0; 5=0; 6=0")
lapop$married_f <-factor(lapop$married_f, levels = c(0,1), labels = c("Single", "Married"))

#Left political ideology
#Continue Variable
lapop$left <- 1+10-(lapop$ideology)
#Factor Variable
lapop$ideology_f=rec(lapop$left, rec="1:4=1; 5:6=2; 7:10=3")
lapop$ideology_f[is.na(lapop$ideology_f)] = 99
lapop$ideology_f <-factor(lapop$ideology_f, levels = c(1,2,3, 99), labels = c("Right", "Center", "Left", "Not declared"))

#Employment
lapop$employment_r=rec(lapop$employment, rec="4=1; 6=1; 3=2; 5=2; 7=2; 1:2=3")
lapop$employment_r <-factor(lapop$employment_r, levels = c(1,2,3), labels = c("No workforce", "Unemployed", "Employee"))

#Education
lapop$education_r=rec(lapop$education, rec="0:6=1; 7:12=2; 13:18=3")
lapop$education_r <-factor(lapop$education_r, levels = c(1,2,3), labels = c("Primary", "Secondary", "Tertiary"))

#Urban Zone
lapop$zone[lapop$zone==2] <- 0
lapop$zone <-factor(lapop$zone, levels = c(0, 1), labels = c("Rural", "Urban"))

#Size City
lapop$tamano <- lapop$sizecity
lapop$sizecity <- 1+5-(lapop$tamano)
lapop$sizecity[lapop$sizecity<1] <- NA
lapop$sizecity <-factor(lapop$sizecity, levels = c(1,2,3,4,5), labels = c("Rural area", "Small city", "Medium city", "Big city", "National capital"))

#Average Trust
lapop$trust <- rowMeans(lapop[c("trustffaa", "trustpolice", "trustjudicial", "trustexecutive", "trustcongress", "trustpolparties")], na.rm=TRUE)

#People in the home: the number of children in the household is taken as a proxy of the variable "people in the home" with a adjust of +1. This is used to estimate per capita income.
lapop$nhome <- lapop$children+1
```

+ Income Variable

```{r Income Variable, echo=TRUE, message=FALSE, warning=FALSE}

#INCOME: The midpoint of the range is imputed for each attribute of the response, according to the scale of each country (currency type). Then the per capita income is obtained by dividing by the number of people in the household, it is transformed into a logarithm and finally, it is transformed into quintiles and deciles.

#--------------------------------------------------------------------#
#Mexico: cod. 1
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="MEX"]<- rec(lapop$income,rec="1=400;2=1201;3=2001;4=2801;5=3601;6=4701;7=6101;8=8401;9=11751;10=13500;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="MEX"]<- rec(lapop$income,rec="1=400;2=1201;3=2001;4=2801;5=3601;6=4701;7=6101;8=8401;9=11751;10=13500;
88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="MEX"]<- rec(lapop$income,rec="1=240;2=600;3=841;4=1201;5=1796;6=2511;7=3231;8=3951;9=5386;10=6821;11=7896;12=9331;13=10771;14=12206;15=13641;16=14360;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="MEX"]<- rec(lapop$income,rec="1=350;2=875;3=1276;4=1676;5=2026;6=2476;7=3001;8=3476;9=3901;10=4276;11=4926;12=5926;13=6851;14=8001;15=9951;16=11150;88=NA;98=NA")

#--------------------------------------------------------------------#
#Guatemala: cod. 2
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="GTM"]<- rec(lapop$income,rec="1=500;2=1251;3=1751;4=2251;5=2901;6=3651;7=4501;8=5801;9=8051;10=9500;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="GTM"]<- rec(lapop$income,rec="1=500;2=1251;3=1751;4=2251;5=2901;6=3651;7=4501;8=5801;9=8051;10=9500;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="GTM"]<- rec(lapop$income,rec="1=180;2=540;3=901;4=1261;5=1621;6=1981;7=2346;8=2701;9=3056;10=3511;11=4051;12=4861;13=5941;14=7561;15=9726;16=10810;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="GTM"]<- rec(lapop$income,rec="1=180;2=480;3=721;4=961;5=1201;6=1441;7=1681;8=1921;9=2161;10=2401;11=2696;12=3056;13=3511;14=4051;15=4861;16=5400;88=NA;98=NA")

#--------------------------------------------------------------------#
#El salvador: cod. 3
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="SLV"]<- rec(lapop$income,rec="1=23;2=68;3=118;4=217;5=361;6=505;7=649;8=865;9=1225;10=1441;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="SLV"]<- rec(lapop$income,rec="1=23;2=68;3=118;4=217;5=361;6=505;7=649;8=865;9=1225;10=1441;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="SLV"]<- rec(lapop$income,rec="1=15;2=40;3=56;4=76;5=116;6=161;7=226;8=316;9=406;10=496;11=586;12=721;13=946;14=1261;15=1621;16=1800;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="SLV"]<- rec(lapop$income,rec="1=20;2=48;3=66;4=88;5=113;6=138;7=161;8=186;9=218;10=253;11=298;12=353;13=411;14=496;15=666;16=780;88=NA;98=NA")

#--------------------------------------------------------------------#
#HONDURAS: cod. 4
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="HND"]<- rec(lapop$income,rec="1=238;2=713;3=1426;4=2376;5=3326;6=4751;7=6651;8=8551;9=11876;10=14251;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="HND"]<- rec(lapop$income,rec="1=500;2=1751;3=3501;4=5501;5=7501;6=10501;7=13501;8=16001;9=19001;10=20501;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="HND"]<- rec(lapop$income,rec="1=460;2=1375;3=2291;4=3211;5=4126;6=5041;7=6411;8=7786;9=8941;10=10316;11=11691;12=13756;13=16506;14=19256;15=22691;16=24750;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="HND"]<- rec(lapop$income,rec="1=1025;2=2475;3=3176;4=3725;5=4275;6=4826;7=5376;8=6051;9=6926;10=7801;11=8701;12=9826;13=10976;14=12201;15=14676;16=16450;88=NA;98=NA")

#--------------------------------------------------------------------#
#Nicaragua: cod. 5
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="NIC"]<- rec(lapop$income,rec="1=750;2=2251;3=3626;4=4876;5=7001;6=10626;7=14876;8=19126;9=23376;10=25500;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="NIC"]<- rec(lapop$income,rec="1=750;2=2251;3=3626;4=4876;5=7001;6=10626;7=14876;8=19126;9=23376;10=25500;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="NIC"]<- rec(lapop$income,rec="1=335;2=835;3=1171;4=1671;5=2336;6=2841;7=3181;8=3681;9=4511;10=5511;11=7016;12=10026;13=14036;14=18045;15=22055;16=24060;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="NIC"]<- rec(lapop$income,rec="1=550;2=1375;3=1801;4=2251;5=2776;6=3101;7=3351;8=3726;9=4201;10=4776;11=5426;12=6051;13=7026;14=8751;15=11651;16=13500;88=NA;98=NA")

#--------------------------------------------------------------------#
#Costa Rica: cod. 6
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="CRI"]<- rec(lapop$income,rec="1=5000;2=55000;3=117500;4=155000;5=197500;6=245000;7=310000;8=400000;9=575000;10=700000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="CRI"]<- rec(lapop$income,rec="1=53500;2=141528;3=203103;4=264869;5=335294;6=422001;7=539601;8=719638;9=1029788;10=1226501;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="CRI"]<- rec(lapop$income,rec="1=32060;2=80146;3=112201;4=160291;5=224086;6=272171;7=304871;8=352956;9=432776;10=528951;11=625126;12=721296;13=817466;14=961726;15=1154071;16=1250241;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="CRI"]<- rec(lapop$income,rec="1=44300;2=103151;3=134551;4=172976;5=209776;6=239326;7=266876;8=292626;9=318576;10=357176;11=417926;12=494551;13=581401;14=701901;15=917850;16=1059100;88=NA;98=NA")

#--------------------------------------------------------------------#
#Panama: cod. 7
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="PAN"]<- rec(lapop$income,rec="1=50;2=150;3=300;4=500;5=700;6=900;7=1250;8=2000;9=3750;10=5000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="PAN"]<- rec(lapop$income,rec="1=50;2=150;3=300;4=500;5=700;6=900;7=1250;8=2000;9=3750;10=5000;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="PAN"]<- rec(lapop$income,rec="1=60;2=145;3=201;4=291;5=406;6=491;7=551;8=636;9=781;10=956;11=1216;12=1736;13=2431;14=3126;15=3815;16=4160;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="PAN"]<- rec(lapop$income,rec="1=75;2=176;3=226;4=276;5=326;6=376;7=421;8=456;9=491;10=526;11=571;12=641;13=736;14=856;15=1010;16=1100;88=NA;98=NA")

#--------------------------------------------------------------------#
#Colombia: cod. 8
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="COL"]<- rec(lapop$income,rec="1=45500;2=136000;3=271000;4=541000;5=860500;6=1250001;7=1750001;8=2500001;9=3500001;10=4000001;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="COL"]<- rec(lapop$income,rec="1=45500;2=136000;3=271000;4=541000;5=860500;6=1250001;7=1750001;8=2500001;9=3500001;10=4000001;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="COL"]<- rec(lapop$income,rec="1=45000;2=135000;3=225000;4=315000;5=405000;6=495000;7=585000;8=670000;9=755000;10=870000;11=1020000;12=1350000;13=1850000;14=2650000;15=3750000;16=4300000;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="COL"]<- rec(lapop$income,rec="1=80000;2=205000;3=295001;4=380001;5=450001;6=510001;7=565001;8=620001;9=685001;10=765001;11=885001;12=1030001;13=1250001;14=1650001;15=2550000;16=3200000;88=NA;98=NA")

#--------------------------------------------------------------------#
#Ecuador: cod. 9
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="ECU"]<- rec(lapop$income,rec="1=30;2=81;3=151;4=251;5=401;6=626;7=876;8=1251;9=1751;10=2001;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="ECU"]<- rec(lapop$income,rec="1=30;2=81;3=151;4=251;5=401;6=626;7=876;8=1251;9=1751;10=2001;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="ECU"]<- rec(lapop$income,rec="1=20;2=65;3=111;4=156;5=201;6=241;7=286;8=331;9=376;10=431;11=496;12=661;13=926;14=1321;15=1845;16=2110;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="ECU"]<- rec(lapop$income,rec="1=65;2=155;3=201;4=241;5=273;6=298;7=331;8=363;9=388;10=431;11=496;12=596;13=726;14=926;15=1320;16=1580;88=NA;98=NA")

#--------------------------------------------------------------------#
#Bolivia: cod. 10
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="BOL"]<- rec(lapop$income,rec="1=125;2=375;3=650;4=1001;5=1601;6=2501;7=4001;8=7500;9=15000;10=20000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="BOL"]<- rec(lapop$income,rec="1=125;2=375;3=650;4=1001;5=1601;6=2501;7=4001;8=7500;9=15000;10=20000;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="BOL"]<- rec(lapop$income,rec="1=70;2=205;3=341;4=476;5=611;6=751;7=886;8=1016;9=1151;10=1326;11=1531;12=2041;13=2856;14=4891;15=8150;16=9780;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="BOL"]<- rec(lapop$income,rec="1=125;2=375;3=651;4=951;5=1251;6=1551;7=1851;8=2201;9=2601;10=3051;11=3651;12=4401;13=5301;14=6401;15=8500;16=10000;88=NA;98=NA")

#--------------------------------------------------------------------#
#Peru: cod. 11
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="PER"]<- rec(lapop$income,rec="1=50;2=151;3=301;4=501;5=701;6=1001;7=1401;8=1801;9=2500;10=3000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="PER"]<- rec(lapop$income,rec="1=50;2=151;3=301;4=501;5=701;6=1001;7=1401;8=1801;9=2500;10=3000;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="PER"]<- rec(lapop$income,rec="1=55;2=170;3=286;4=396;5=506;6=621;7=736;8=846;9=956;10=1096;11=1266;12=1691;13=2366;14=3041;15=3715;16=4050;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="PER"]<- rec(lapop$income,rec="1=105;2=270;3=381;4=481;5=571;6=656;7=741;8=821;9=896;10=961;11=1031;12=1141;13=1296;14=1616;15=2215;16=2580;88=NA;98=NA")

#--------------------------------------------------------------------#
#Paraguay: cod. 12
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="PRY"]<- rec(lapop$income,rec="1=68500;2=206001;3=412501;4=687501;5=962501;6=1375001;7=1925001;8=2475001;9=3437501;10=4125001;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="PRY"]<- rec(lapop$income,rec="1=68500;2=206001;3=412501;4=687501;5=962501;6=1375001;7=1925001;8=2475001;9=3437501;10=4125001;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="PRY"]<- rec(lapop$income,rec="1=69095;2=207290;3=345486;4=483676;5=690966;6=967356;7=1243746;8=1520131;9=1931946;10=2346526;11=2628441;12=2902066;13=3175691;14=3731231;15=4560390;16=4974970;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="PRY"]<- rec(lapop$income,rec="1=138195;2=414580;3=690966;4=967356;5=1220711;6=1451031;7=1681356;8=1911681;9=2142001;10=2372321;11=2628441;12=2902066;13=3175691;14=3731231;15=4560390;16=4974970;88=NA;98=NA")

#--------------------------------------------------------------------#
#Chile: cod. 13
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="CHL"]<- rec(lapop$income,rec="1=43000;2=122500;3=181500;4=241500;5=297000;6=374500;7=444000;8=492500;9=672000;10=813000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="CHL"]<- rec(lapop$income,rec="1=43000;2=122500;3=181500;4=241500;5=297000;6=374500;7=444000;8=492500;9=672000;10=813000;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="CHL"]<- rec(lapop$income,rec="1=15165;2=45500;3=75836;4=106166;5=136501;6=166836;7=197471;8=227501;9=257531;10=295751;11=341251;12=409501;13=500501;14=637001;15=819000;16=910000;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="CHL"]<- rec(lapop$income,rec="1=61150;2=141300;3=172526;4=195276;5=218151;6=246526;7=277801;8=308676;9=344126;10=390626;11=446326;12=507876;13=591151;14=703901;15=850950;16=936000;88=NA;98=NA")

#--------------------------------------------------------------------#
#Uruguay: cod. 14
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="URY"]<- rec(lapop$income,rec="1=2250;2=5251;3=7001;4=9001;5=11001;6=13001;7=16001;8=20501;9=28001;10=33001;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="URY"]<- rec(lapop$income,rec="1=2250;2=5251;3=7001;4=9001;5=11001;6=13001;7=16001;8=20501;9=28001;10=33001;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="URY"]<- rec(lapop$income,rec="1=1000;2=2500;3=3501;4=5001;5=6991;6=8491;7=9511;8=11011;9=13501;10=16501;11=19501;12=22501;13=25501;14=28501;15=31500;16=33000;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="URY"]<- rec(lapop$income,rec="1=2450;2=6025;3=8076;4=9551;5=10801;6=12351;7=14226;8=16276;9=18301;10=20726;11=23701;12=27101;13=30101;14=32351;15=34550;16=35650;88=NA;98=NA")

#--------------------------------------------------------------------#
#Brasil: cod. 15
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="BRA"]<- rec(lapop$income,rec="1=19000;2=57001;3=95001;4=152001;5=228001;6=285001;7=380001;8=513001;9=665001;10=760001;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="BRA"]<- rec(lapop$income,rec="1=25500;2=76501;3=127501;4=204001;5=306001;6=382501;7=510001;8=688501;9=892501;10=1020001;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="BRA"]<- rec(lapop$income,rec="1=50;2=155;3=261;4=361;5=466;6=571;7=676;8=776;9=876;10=986;11=1086;12=1186;13=1321;14=1476;15=1705;16=1860;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="BRA"]<- rec(lapop$income,rec="1=250;2=601;3=751;4=851;5=951;6=1051;7=1151;8=1301;9=1501;10=1701;11=1901;12=2551;13=3701;14=4851;15=6001;16=6601;88=NA;98=NA")

#--------------------------------------------------------------------#
#Venezuela: cod. 16
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="VEN"]<- rec(lapop$income,rec="1=185;2=493;3=923;4=1536;5=2146;6=2761;7=3386;8=4001;9=4601;10=4901;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="VEN"]<- rec(lapop$income,rec="1=185;2=493;3=923;4=1536;5=2146;6=2761;7=3386;8=4001;9=4601;10=4901;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="VEN"]<- rec(lapop$income,rec="1=255;2=774;3=1293;4=1741;5=2128;6=2515;7=2902;8=3289;9=3676;10=4063;11=4450;12=4899;13=5418;14=5936;15=6447;16=6702;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="VEN"]<- rec(lapop$income,rec="1=850;2=1901;3=2301;4=2626;5=2876;6=3176;7=3551;8=3951;9=4326;10=4651;11=5051;12=5651;13=6276;14=6976;15=8250;16=9100;88=NA;98=NA")

#--------------------------------------------------------------------#
#Argentina: cod. 17
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="ARG"]<- rec(lapop$income,rec="1=250;2=751;3=1251;4=1751;5=2251;6=2751;7=3251;8=3751;9=4250;10=4500;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="ARG"]<- rec(lapop$income,rec="1=600;2=1601;3=2301;4=2951;5=3751;6=4701;7=6001;8=7851;9=10950;10=13000;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="ARG"]<- rec(lapop$income,rec="1=385;2=960;3=1341;4=1916;5=2681;6=3256;7=3646;8=4221;9=5176;10=6326;11=7476;12=8626;13=9776;14=10926;15=12650;16=13800;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="ARG"]<- rec(lapop$income,rec="1=650;2=1500;3=1851;4=2201;5=2601;6=2951;7=3301;8=3651;9=3951;10=4301;11=4801;12=5451;13=6301;14=7401;15=8950;16=9900;88=NA;98=NA")

#--------------------------------------------------------------------#
#Republica Dominicana: cod. 18
#2008
lapop$income_r[lapop$year==2008 & lapop$country=="DOM"]<- rec(lapop$income,rec="1=438;2=1313;3=2626;4=4376;5=6126;6=8751;7=12251;8=15751;9=21876;10=38125;11=50000;88=NA;98=NA")
#2010
lapop$income_r[lapop$year==2010 & lapop$country=="DOM"]<- rec(lapop$income,rec="1=1425;2=4288;3=6863;4=9151;5=11901;6=15001;7=22501;8=34501;9=50650;10=60800;88=NA;98=NA")
#2012
lapop$income_r[lapop$year==2012 & lapop$country=="DOM"]<- rec(lapop$income,rec="1=505;2=1510;3=2516;4=3521;5=4526;6=5536;7=7036;8=8541;9=9806;10=11316;11=15091;12=21126;13=36211;14=54316;15=66385;16=72420;88=NA;98=NA")
#2014
lapop$income_r[lapop$year==2014 & lapop$country=="DOM"]<- rec(lapop$income,rec="1=1400;2=3400;3=4601;4=5576;5=6451;6=7601;7=8801;8=10076;9=11251;10=12151;11=14026;12=16876;13=20201;14=25851;15=37875;16=46150;88=NA;98=NA")

#--------------------------------------------------------------------#
#Income per cápita
lapop$income_pc=lapop$income_r/lapop$nhome
lapop$income_pc=log(lapop$income_pc) #Log income per cápita

#Quintiles de income
lapop = 
  lapop %>% 
  group_by(country, year) %>%
  mutate(quintil=ntile(income_pc,5))

#Deciles de income
lapop = 
  lapop %>% 
  group_by(country, year) %>%
  mutate(decil=ntile(income_pc,10))
```

+ Quintile and Decil as factors and dummy ("missing" category is added) 

```{r Quintil/Decil, echo=TRUE, message=FALSE, warning=FALSE}
lapop=as.data.frame(lapop) #Transformation to Data Frame 
#Quintil
lapop$quintil_f <- lapop$quintil
lapop$quintil_f [is.na(lapop$quintil_f)] <- 6 #Missing
lapop$quintil_f <- as.factor(lapop$quintil_f)
#Decil
lapop$decil_f <- lapop$decil
lapop$decil_f [is.na(lapop$decil_f)] <- 11 #Missing
lapop$quintil_f <- as.factor(lapop$quintil_f)

#Dummies 
#Quintil
lapop$quintil_d <- lapop$quintil_f
lapop$quintil_d=rec(lapop$quintil_d, rec="1:3=0; 4:5=1; 6=0")
lapop$quintil_d <-factor(lapop$quintil_d, levels = c(0,1), labels = c("Not decil 4-5", "Decil 4-5"))
#Decil
lapop$decil_d <- lapop$decil_f
lapop$decil_d=rec(lapop$decil_d, rec="1:8=0; 9:10=1; 11=0")
lapop$decil_d <-factor(lapop$decil_d, levels = c(0,1), labels = c("Not decil 9-10", "Decil 9-10"))
```

+ Time dummy

```{r Time Dummy, echo=TRUE, message=FALSE, warning=FALSE}
#2008
lapop$year_2008 <- 0
lapop$year_2008[lapop$year==2008] <- 1
#2008
lapop$year_2010 <- 0
lapop$year_2010[lapop$year==2010] <- 1
#2008
lapop$year_2012 <- 0
lapop$year_2012[lapop$year==2012] <- 1
#2008
lapop$year_2014 <- 0
lapop$year_2014[lapop$year==2014] <- 1
```

+ Variables Level 2 and Merge with variables level 1

```{r Variables Level 2 and Merge, echo=TRUE, message=FALSE, warning=FALSE}
n2 <- read_dta("Databases/N2.dta") #Directory and database
n2=as.data.frame(n2) #Transformation to Data Frame 
names(n2)= tolower(names(n2)) #Name of the variables in minuscule
colnames(n2) <- c("country", "year", "country_name", "gini","pib", "gsocial", "unemployment", "informal", "welfare", "welfare1", "div_ethnic", "div_religion", "div_language", "gini_mean", "pib_mean", "gsocial_mean", "unemployment_mean", "informal_mean", "gini_dif", "pib_dif", "gsocial_dif", "unemployment_dif", "informal_dif" )
#Countries of the sample (Labels)
n2$country <- factor(n2$country,
levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18),
labels = c("MEX", "GTM", "SLV", "HND", "NIC", "CRI", "PAN", "COL", "ECU", "BOL", "PER", "PRY", "CHL", "URY", "BRA", "VEN", "ARG", "DOM"))
#Imput variable Year
n2$year <- as.factor(n2$year)
#Merge Data level 1 and level 2
lapop <- merge(lapop, n2, by=c("country", "year"))
```

+ Missing values: Consider the variables with complete information

```{r Missing Data, echo=TRUE, message=FALSE, warning=FALSE}
#Listwise with the variables used to estimate the models
lapop = lapop[complete.cases(lapop$redistribution, lapop$man, lapop$age, lapop$married_f, lapop$ideology_f, lapop$employment_r, lapop$education_r, lapop$decil_f, lapop$decil_d, lapop$quintil_f, lapop$quintil_d, lapop$zone, lapop$trust),]
```

+ Databases paper

```{r Save and Load Databases, echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE}
#Save Databases paper
save(n2, file = "Databases/Databases_paper/n2.RData")
save(lapop, file = "Databases/Databases_paper/lapop.RData") #Merge Data level 1 and level 2
load("Databases/Databases_paper/lapop.RData") #Load Database
lapop <- lapop[order(lapop$country), ] #Order the database
```

+ Descriptive statistics

```{r Descriptive statistics, echo=TRUE, message=FALSE, warning=FALSE}
lapop1 = lapop %>% dplyr::select(country, country_name, country_wave, year, wave, id, weight1500, wt, redistribution, man, age, married_f, ideology_f, employment_r, education_r, decil_f, decil_d, quintil_f, quintil_d, zone, trust, pib_mean, pib_dif, gini_mean, gini_dif, welfare, year) #Specific database for analyzes
#Table2: Descriptive statistics
describe(lapop1)
stargazer(lapop1,title="Descriptive Statistics", type = "text")
```

+ Descriptive figures of the dependent variable: redistribution

```{r Descriptive figure 2, echo=TRUE, message=FALSE, warning=FALSE}
#Database: agreement with redistribution by countries
redist <- xtabs(~country+redistribution, data=lapop1)
redist <- prop.table(redist, 1)*100
redist <- as.data.frame(redist)
redist$order <- factor(redist$country,
levels = c("BOL","VEN","PER","GTM","HND","ECU","PAN","SLV","MEX","COL","BRA","CRI","CHL","NIC","URY","DOM","ARG","PRY"),
labels = c(18:1))
redist <-redist[order(redist$order, redist$redistribution),]
rownames(redist) <- 1:nrow(redist)

#Figure 2: Agreement with redistribution by countries
figure2 = ggplot(data = redist, aes(x=reorder(country, redistribution), y = Freq, fill = redistribution)) + geom_bar(stat = "identity") + coord_flip() + labs(x = "", y = "", fill="") +
  scale_fill_grey(start = .9, end = .25, name=" ") + 
  scale_y_continuous(labels = function(Freq){paste0(Freq, "%") }) +
  theme(panel.background = element_rect(fill = "white"), 
        axis.text=element_text(size=10),
        strip.text=element_text(size=12))
figure2
ggsave("Graphs/Figure2.png", plot=figure2)
```


```{r Descriptive figure 3, echo=TRUE, message=FALSE, warning=FALSE}
lapop1=as.data.frame(lapop1) #Transformation to Data Frame
#Figure 3: agreement with redistribution by countries and wave
figure3 = ggplot(lapop1, aes(x= redistribution,  colour= year)) + 
  geom_line(aes(y = ..prop.., fill = factor(..x..)), stat="count", binwidth = 1, size = 1) +
  facet_wrap(~country_name, ncol=3) +
  xlim(1, 7) +
  labs(x = "Agreement with redistribution", y = "", fill="") +
  scale_color_grey(start=0.8, end=0.2, name="Wave") +  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1:7)) +
  theme(axis.text=element_text(size=15),
        strip.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size=unit(1,"cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  theme_hc()
figure3
ggsave("Graphs/Figure3.png", plot=figure3, height = 10, width = 10, units="in")
```


+ Average redistribution by quintil/decile of income by country

```{r Average redistribution, echo=TRUE, message=FALSE, warning=FALSE}
#Table3: Average redistribution by quintil of income by country
redis_quintil = with(lapop1, tapply(redistribution, list(country, quintil_f), mean))
redis_quintil = as.data.frame(redis_quintil)
redis_quintil$total <- rowMeans(redis_quintil[c("1", "2", "3", "4", "5", "6")])
stargazer(redis_quintil)

#Table3: Average redistribution by decil of income by country
redis_decil = with(lapop1, tapply(redistribution, list(country, decil_f), mean))
redis_decil = as.data.frame(redis_decil)
redis_decil$total <- rowMeans(redis_decil[c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11")])
stargazer(redis_decil)
```

+ Multilevel estimation: Weighted models

```{r Multilevel estimation, echo=TRUE, message=FALSE, warning=FALSE}
#Null model: Country Year
model_w0 = lmer(redistribution ~ 1 + (1 | country_wave) + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w0)
#Intraclass correlation estimation (ICC)
varcomp=as.data.frame(VarCorr(model_w0))
varcomp
o2country=varcomp[2,4] 
o2country_year=varcomp[1,4]
o2individual=varcomp[3,4] 
icc_country=o2country/(o2country+o2country_year+o2individual)
icc_country_year=o2country_year/(o2country+o2country_year+o2individual)
icc_individual=o2individual/(o2country+o2country_year+o2individual)

icc_country #0.04099855
icc_country_year #0.03440511
icc_individual #0.9245963

#1.Model: redistribution
model_w1 = lmer(redistribution ~ 1 + decil_d 
                + year + (1 | country_wave) 
                + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w1)

##2.Model: individual predictors (H4)
model_w2 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + year + (1 | country_wave) + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w2)

###3. Model: individual predictors + GINI (H1 Y H2)
model_w3 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif + year
                + (1 | country_wave) + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w3)

####4 Model: individual predictors + GINI  + PIB
model_w4 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif
                + pib_mean + pib_dif + year
                + (1 | country_wave) + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w4)

#####5. Model: individual predictors + GINI  + PIB + Welfare State (H3)
model_w5 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif
                + pib_mean + pib_dif
                + welfare + year
                + (1 | country_wave) + (1 | country_name), data=lapop1, weights=wt)
screenreg(model_w5)

######6. Model: country, country_year  Model: individual predictors + GINI + PIB (Decil Aleatorio) (H4.2)
model_w6 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif
                + pib_mean + pib_dif + year
                + (1 + decil_d| country_wave) + (1 + decil_d | country_name), data=lapop1, weights=wt)
screenreg(model_w6)

#######7. Model: country, country_year  Model: individual predictors + GINI + PIB (Decil Aleatorio) (Decil*GINI) (H5)
model_w7 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif
                + pib_mean + pib_dif
                + decil_d*gini_mean + decil_d*gini_dif + year
                + (1 + decil_d | country_wave) + (1 + decil_d | country_name), data=lapop1, weights=wt)
screenreg(model_w7)

########8. Model: country, country_year  Model: individual predictors + GINI + PIB (Decil Aleatorio) (Decil*PIB)
model_w8 = lmer(redistribution ~ 1 + decil_d + man + age + married_f 
                + ideology_f + trust + employment_r + education_r 
                + zone + gini_mean + gini_dif
                + pib_mean + pib_dif
                + decil_d*pib_mean + decil_d*pib_dif + year
                + (1 + decil_d | country_wave) + (1 + decil_d | country_name), data=lapop1, weights=wt)
screenreg(model_w8)

#All Weighted models
texreg(list(model_w0, model_w1, model_w2, model_w3, model_w4, 
            model_w5, model_w6, model_w7, model_w8), 
       stars = c(0.01,0.05,0.1), dcolumn = TRUE)
```

+ Multilevel estimation: aditional analisys 

```{r Aditional multilevel estimation, echo=TRUE, message=FALSE, warning=FALSE}
#LR Test: Test Modelos Anidados (Considera modelos con ponderadores)
lrtest (model_w0, model_w1)
lrtest (model_w1, model_w2)
lrtest (model_w2, model_w3)
lrtest (model_w3, model_w4)
lrtest (model_w4, model_w5)
lrtest (model_w5, model_w6)
lrtest (model_w6, model_w7)
lrtest (model_w7, model_w8)
```

+ Multilevel estimation: Figura 4. Random effect of income decile on agreement with redistribution by country (Model includes weight)

```{r Figure 4 random effect, echo=TRUE, message=FALSE, warning=FALSE}
#Figura 4: Random effect of income decile on agreement with redistribution by country (Model includes weight) 
figure4=plot_model(model_w6, type = "re", 
           show.legend = TRUE,
           show.values = TRUE,
           facet.grid = TRUE,
           y.offset = .4,
           value.offset = .4,
           value.size = 3.5,
           color="darkgray",
           sort.est= "decil_dDecil 9-10",
           title="Random effect of income decile on agreement with redistribution by country: intercept and slope")
ranef(model_w6)
figure4
```

+ Multilevel estimation: Figura 5. Marginal Effects

```{r Figure 5 marginal effects, echo=TRUE, message=FALSE, warning=FALSE}
#Figure 5: Marginal effects
#GINI (Mean)
figure5a = interplot(m = model_w7, var1 = 'decil_d', var2 = 'gini_mean', ci = 0.90) + 
  xlab("Gini (Mean)") +
  ylab("Change in Agreement with Redistribution") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
figure5a
ggsave("Graphs/Figure5a.png", plot=figure5a)
#GINI (Change)
figure5b = interplot(m = model_w7, var1 = 'decil_d', var2 = 'gini_dif', ci = 0.90) + 
  xlab("Gini (Change)") +
  ylab("Change in Agreement with Redistribution") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
figure5b
ggsave("Graphs/Figure5b.png", plot=figure5b)

#PIB (Mean)
figure5c = interplot(m = model_w8, var1 = 'decil_d', var2 = 'pib_mean', ci = 0.90) + 
  xlab("PIB (Promedio)") +
  ylab("Change in Agreement with Redistribution") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
figure5c
ggsave("Graphs/Figure5c.png", plot=figure5c)
#PIB (Change)
figure5d = interplot(m = model_w8, var1 = 'decil_d', var2 = 'pib_dif', ci = 0.90) + 
  xlab("PIB (Cambio)") +
  ylab("Change in Agreement with Redistribution") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
figure5d
ggsave("Graphs/Figure5d.png", plot=figure5d)
```

+ Tables and complementary figures (annex)

```{r Tables and complementary figures, echo=TRUE, message=FALSE, warning=FALSE}
#6.1# Table4: Sample: Observations by country and year
lapop1$cases=1
sample = with(lapop1, tapply(cases, list(country_name, year), sum))
sample = as.data.frame(sample)
sample$total <- rowSums(sample[c("2008", "2010", "2012", "2014")])
sample_country = as.data.frame(with(lapop1, tapply(cases, list(year), sum)))
xtable(sample, digits = 0)
xtable(sample_country, digits = 0)

#6.2# Figured: Income deciles and agreement with redistribution, by country and year
decil_redist = aggregate(lapop1, by=list(lapop$country_name, lapop$year, lapop$decil_f), FUN=mean, na.rm=TRUE) 
decil_redist$country_name = decil_redist$Group.1
decil_redist$year = as.factor(decil_redist$Group.2)
decil_redist$decil_f = decil_redist$Group.3

figure62 = ggplot(decil_redist, aes(x=decil_f, y=as.numeric(redistribution), colour=as.factor(year))) + 
  geom_line(stat="identity", binwidth = 1, size = 1) +
  facet_wrap(~country_name, ncol=3) +
  scale_colour_grey(start = .7, end = .25, name="Wave") +
  scale_y_continuous(breaks = c(1:7), limits = c(1,7))+
  labs(x = "Income (Deciles)", y = "Agreement with Redistribution", fill="") +
  theme(axis.text=element_text(size=15),
        strip.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size=unit(1,"cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  theme_hc()
figure62
ggsave("Graphs/Figure62.png", plot=figure62, height = 10, width = 10, units="in")
```

```{r Aditional figures, echo=TRUE, message=FALSE, warning=FALSE}
#6.3#Inequality: Relationship "between" countries
between = aggregate(lapop1, by=list(lapop1$country), FUN=mean, na.rm=TRUE) 
figure63= ggplot(between, 
            aes(x= gini_mean, y= redistribution, label=Group.1)) +
  geom_point() + 
  geom_smooth(method = "lm", colour="black") +
  geom_text(aes(label=Group.1),hjust=-0.1, vjust=-0.5, size=3.5) +
  labs(x = "Inequality", y = "Agreement with Redistribution", fill="") +
  scale_y_continuous("Agreement with Redistribution", limits = c(4.8,6.2), 
                     breaks = c(4.8,5.2,5.6,6)) +
  scale_x_continuous("Inequality", limits = c(40,60),
                     breaks = c(40,50,60)) +    
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
figure63
ggsave("Graphs/Figure63.png", plot=figure63)

#6.4#Inequality: Relationship "within" countries
lapop$welfare1=as.factor(lapop$welfare)
lapop$welfare1=as.integer(lapop$welfare1)
lapop_country_year = aggregate(lapop, 
                          by=list(lapop$country, lapop$country_name, lapop$wave, 
                                  lapop$country_wave), FUN=mean, na.rm=TRUE) 
lapop_country_year1 = select(lapop_country_year,Group.1, Group.2, Group.3, Group.4,
                      redistribution, gini_mean, pib_mean, trust, gsocial, unemployment, informal, welfare1)
colnames(lapop_country_year1) <- c("country", "country_name", "wave", "country_wave", "redistribution", "gini", "pib", "trust", "gsocial", "unemployment", "informal", "welfare")

figure64= ggplot(lapop_country_year1, 
            aes(x= gini, y= redistribution, colour=as.factor(wave+2000))) +
  geom_point(size = 2, alpha = .8) + 
  stat_smooth(size = 1, method = "lm", se = FALSE) +
  facet_wrap(~country_name, ncol=3) +
  labs(x = "Inequality", y =  "Agreement with Redistribution") + 
  scale_color_grey(start=0.8, end=0.2, name="Wave") +  
  scale_y_continuous( "Agreement with Redistribution", limits = c(4.2,6.7), 
                     breaks = c(4.5,5,5.5,6,6.5)) +
  scale_x_continuous("Inequality",
                     breaks = c(35,40,45,50,55,60)) +  
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="bottom",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
figure64
ggsave("Graphs/Figure64.png", plot=figure64, height = 10, width = 10, units="in")

#6.5#Economic Development: Relationship "between" countries
figure65 = ggplot(between, 
            aes(x= pib_mean, y= redistribution, label=Group.1)) +
  geom_point() + 
  geom_smooth(method = "lm", colour="black") +
  geom_text(aes(label=Group.1),hjust=-0.1, vjust=-0.5, size=3.5) +
  labs(x = "Economic Development", y = "Agreement with Redistribution", fill="") +
  scale_y_continuous("Agreement with Redistribution", limits = c(4.8,6.2), 
                     breaks = c(4.8,5.2,5.6,6)) +
  scale_x_continuous("Economic Development", limits = c(1,14),
                     breaks = c(5,10,15)) +  
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
figure65
ggsave("Graphs/Figure65.png")


#6.6#Economic Development: Relationship "within" countries
figure66= ggplot(lapop_country_year1, 
            aes(x= pib, y= redistribution, colour=as.factor(wave+2000))) +
  geom_point(size = 2, alpha = .8) + 
  geom_smooth(size = 1, method = "lm", se = FALSE) +
  facet_wrap(~country_name, ncol=3) +
  labs(x = "Economic Development", y =  "Agreement with Redistribution") + 
  scale_color_grey(start=0.8, end=0.2, name="Wave") +  
  scale_y_continuous( "Agreement with Redistribution", limits = c(4.2,6.7), 
                     breaks = c(4.5,5,5.5,6,6.5)) +
  scale_x_continuous("Economic Development",
                     breaks = c(0,5,10,15,20)) +  
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="bottom",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
figure66
ggsave("Graphs/Figure66.png", plot=figure66, height = 10, width = 10, units="in")
```

