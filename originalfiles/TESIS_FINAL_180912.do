********************************* TESIS *********************************
************************** GONZALO FRANETOVIC ***************************
********************************* 2017 **********************************




************************* IMPORTACIÓN DE BASE ***************************

/*cd "C:\Users\Gonzalo\Documents\Magister Respaldos\Tesis Respaldos\Bases Madres"
set excelxlsxlargefile on
import delimite using "COMPLETA1_Lapop2.csv", delim (",")

cd "C:\Users\Gonzalo\Documents\Magister Respaldos\Tesis Respaldos\Bases Originales"
save "COMPLETA_Lapop", replace



*************************** REDUCCIÓN DE BASE ***************************

cd "C:\Users\Gonzalo\Documents\Magister Respaldos\Tesis Respaldos\Bases Originales"
use "COMPLETA_Lapop", clear

keep if pais<=21
keep if wave>=2008

recode pais (21=18)

label define pais 1 "Mexico" 2 "Guatemala" 3 "El Salvador" 4 "Honduras" ///
5 "Nicaragua" 6 "Costa Rica" 7 "Panama" 8 "Colombia" 9 "Ecuador" ///
10 "Bolivia" 11 "Peru" 12 "Paraguay" 13 "Chile" 14 "Uruguay" 15 "Brasil" ///
16 "Venezuela" 17 "Argentina" 18"Rep Dominicana"
label values pais pais

cd "C:\Users\Gonzalo\Dropbox\Magister\Tesis\170930 Trabajo\Bases Finales"
save "MUESTRA_Lapop", replace*/



*************************** TRABAJO CON BASE ***************************

*cd "C:\Users\Gonzalo\Dropbox\Magister\Tesis\170930 Trabajo\Bases Finales"
use "MUESTRA_Lapop", clear

*Código Pais_Año
gen pais_r = pais
label define pais_r 1"MEX" 2"GTM" 3"SLV" 4"HND" 5"NIC" 6"CRI" ///
7"PAN" 8"COL" 9"ECU" 10"BOL" 11"PER" 12"PRY" 13"CHL" 14"URY" ///
15"BRA" 16"VEN" 17"ARG" 18"DOM"
label values pais_r pais_r
decode pais_r, generate (pais_r1)
gen wave_r=wave
label define wave_r 2008"08" 2010"10" 2012"12" 2014"14"
label values wave_r wave_r
decode wave_r, generate (wave_r1)
egen pais_año = concat(pais_r1 wave_r1), punct(_)
encode pais_año, gen(pais_año1)

*Ros4 Dicotómica
gen ros4_r=.
replace ros4_r=0 if ros4==1 | ros4==2 | ros4==3 | ros4==4 | ros4==5 | ros4==6
replace ros4_r=1 if ros4==7

*Ros4 Ln
gen ros4_ln=ln(ros4)

*Chequeo: Ros4
table pais wave, contents(count ros4)


*************************** Nivel 1 ***************************

**1.1. Hombre
gen hombre=0
replace hombre=1 if q1==1
*label define hombre 0"Mujer" 1"Hombre"
*label values hombre hombre

**1.2. Edad (+18)
*a. Continua
gen edad=q2 if q2>=18
*b. Cuadrática
gen edad2=edad^2

**1.3 Casado (o conviviente)
recode q11 (2 3 = 1) (1 4 5 6 = 0), gen (casado12)
label define casado12 0"Soltero" 1"Casado"
label values casado12 casado12

recode q11n (2 3 7 = 1) (1 4 5 6 = 0), gen (casado14)
label define casado14 0"Soltero" 1"Casado"
label values casado14 casado14

gen casado=.
replace casado=0 if casado12!=. | casado14!=.
replace casado=1 if casado12==1 | casado14==1
*label define casado 0"Soltero" 1"Casado"
*label values casado casado

drop casado12 casado14

**1.4. Izquierda
*a. Continua
gen izquierda = 0
replace izquierda = 1 + 10 - l1
replace izquierda = . if izquierda<1
*b. Factor
recode l1 (1 2 3 4 = 3) (5 6 = 2) (7 8 9 10 = 1), gen (idpolitica)
label define idpolitica 1"Derecha" 2"Centro" 3"Izquierda"
label values idpolitica idpolitica

**1.5. Situación laboral
recode ocup4a (4 6 = 1) (3 5 7 = 2) (1 2 = 3), gen (sitlaboral)
label define sitlaboral 1"No fuerza laboral" 2"Desempleado" 3"Empleado"
label values sitlaboral sitlaboral
tab ocup4a sitlaboral

**1.6. Educación
gen educacion = .
replace educacion = 1 if (ed<=6)
replace educacion = 2 if (ed>6) & (ed<=12)
replace educacion = 3 if (ed>12)
label define educacion 1"Primaria" 2"Secundaria" 3"Terciaria"
label values educacion educacion


**1.7. Ingreso
ssc install egenmore 

egen group = group(pais)
set more off


****Quintiles
egen quintiles2008 = xtile (q10), by (pais), if wave==2008, nquantiles(5)
egen quintiles2010 = xtile (q10), by (pais), if wave==2010, nquantiles(5)
egen quintiles2012 = xtile (q10new), by (pais), if wave==2012, nquantiles(5)
egen quintiles2014 = xtile (q10new), by (pais), if wave==2014, nquantiles(5)

set more off

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 quintiles2008 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 quintiles2010 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new quintiles2012 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new quintiles2014 if group == `i'
}

**Cambios Manuales
table pais q10 wave
table pais q10new wave
table pais quintiles2008
table pais quintiles2010
table pais quintiles2012
table pais quintiles2014

*2008
replace quintiles2008=. if pais==2 & wave==2008
replace quintiles2008=5 if q10<=10 & pais==2 & wave==2008
replace quintiles2008=4 if q10<=5  & pais==2 & wave==2008
replace quintiles2008=3 if q10<=3  & pais==2 & wave==2008
replace quintiles2008=2 if q10<=2  & pais==2 & wave==2008
replace quintiles2008=1 if q10<=1  & pais==2 & wave==2008

replace quintiles2008=. if pais==5 & wave==2008
replace quintiles2008=5 if q10<=10 & pais==5 & wave==2008
replace quintiles2008=4 if q10<=5  & pais==5 & wave==2008
replace quintiles2008=3 if q10<=4  & pais==5 & wave==2008
replace quintiles2008=2 if q10<=3  & pais==5 & wave==2008
replace quintiles2008=1 if q10<=2  & pais==5 & wave==2008

replace quintiles2008=. if pais==7 & wave==2008
replace quintiles2008=5 if q10<=10 & pais==7 & wave==2008
replace quintiles2008=4 if q10<=5  & pais==7 & wave==2008
replace quintiles2008=3 if q10<=4  & pais==7 & wave==2008
replace quintiles2008=2 if q10<=3  & pais==7 & wave==2008
replace quintiles2008=1 if q10<=2  & pais==7 & wave==2008

replace quintiles2008=. if pais==9 & wave==2008
replace quintiles2008=5 if q10<=10 & pais==9 & wave==2008
replace quintiles2008=4 if q10<=5  & pais==9 & wave==2008
replace quintiles2008=3 if q10<=4  & pais==9 & wave==2008
replace quintiles2008=2 if q10<=3  & pais==9 & wave==2008
replace quintiles2008=1 if q10<=2  & pais==9 & wave==2008

replace quintiles2008=. if pais==10 & wave==2008
replace quintiles2008=5 if q10<=10 & pais==10 & wave==2008
replace quintiles2008=4 if q10<=5  & pais==10 & wave==2008
replace quintiles2008=3 if q10<=4  & pais==10 & wave==2008
replace quintiles2008=2 if q10<=3  & pais==10 & wave==2008
replace quintiles2008=1 if q10<=2  & pais==10 & wave==2008

*2010
replace quintiles2010=. if pais==5 & wave==2010
replace quintiles2010=5 if q10<=10 & pais==5 & wave==2010
replace quintiles2010=4 if q10<=5  & pais==5 & wave==2010
replace quintiles2010=3 if q10<=3  & pais==5 & wave==2010
replace quintiles2010=2 if q10<=2  & pais==5 & wave==2010
replace quintiles2010=1 if q10<=1  & pais==5 & wave==2010

replace quintiles2010=. if pais==8 & wave==2010
replace quintiles2010=5 if q10<=10 & pais==8 & wave==2010
replace quintiles2010=4 if q10<=6  & pais==8 & wave==2010
replace quintiles2010=3 if q10<=4  & pais==8 & wave==2010
replace quintiles2010=2 if q10<=3  & pais==8 & wave==2010
replace quintiles2010=1 if q10<=2  & pais==8 & wave==2010

replace quintiles2010=. if pais==9 & wave==2010
replace quintiles2010=5 if q10<=10 & pais==9 & wave==2010
replace quintiles2010=4 if q10<=5  & pais==9 & wave==2010
replace quintiles2010=3 if q10<=4  & pais==9 & wave==2010
replace quintiles2010=2 if q10<=3  & pais==9 & wave==2010
replace quintiles2010=1 if q10<=2  & pais==9 & wave==2010

replace quintiles2010=. if pais==16 & wave==2010
replace quintiles2010=5 if q10<=10 & pais==16 & wave==2010
replace quintiles2010=4 if q10<=5  & pais==16 & wave==2010
replace quintiles2010=3 if q10<=4  & pais==16 & wave==2010
replace quintiles2010=2 if q10<=3  & pais==16 & wave==2010
replace quintiles2010=1 if q10<=2  & pais==16 & wave==2010

*2012
replace quintiles2012=. if pais==15 & wave==2012
replace quintiles2012=5 if q10new<=16 & pais==15 & wave==2012
replace quintiles2012=4 if q10new<=15 & pais==15 & wave==2012
replace quintiles2012=3 if q10new<=13 & pais==15 & wave==2012
replace quintiles2012=2 if q10new<=10 & pais==15 & wave==2012
replace quintiles2012=1 if q10new<=7  & pais==15 & wave==2012

*2014
replace quintiles2014=5 if q10new==16 & pais==14 & wave==2014

**Agrupar quintiles
gen ingreso5=.
replace ingreso5=1 if quintiles2008==1 | quintiles2010==1 | quintiles2012==1 | quintiles2014==1
replace ingreso5=2 if quintiles2008==2 | quintiles2010==2 | quintiles2012==2 | quintiles2014==2
replace ingreso5=3 if quintiles2008==3 | quintiles2010==3 | quintiles2012==3 | quintiles2014==3
replace ingreso5=4 if quintiles2008==4 | quintiles2010==4 | quintiles2012==4 | quintiles2014==4
replace ingreso5=5 if quintiles2008==5 | quintiles2010==5 | quintiles2012==5 | quintiles2014==5
label define ingreso5 1"Ingreso I" 2"Ingreso II" 3"Ingreso III" 4"Ingreso IV" 5"Ingreso V"
label values ingreso5 ingreso5

table pais ingreso5 wave
table pais ingreso5

****Cuartiles
egen cuartiles2008 = xtile (q10), by (pais), if wave==2008, nquantiles(4)
egen cuartiles2010 = xtile (q10), by (pais), if wave==2010, nquantiles(4)
egen cuartiles2012 = xtile (q10new), by (pais), if wave==2012, nquantiles(4)
egen cuartiles2014 = xtile (q10new), by (pais), if wave==2014, nquantiles(4)


su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 cuartiles2008 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 cuartiles2010 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new cuartiles2012 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new cuartiles2014 if group == `i'
}


/*Cambios Manuales

*Brasil

*2008
replace cuartiles2008=1 if q10<=1 & pais==15 & wave==2008
replace cuartiles2008=2 if q10==2 & pais==15 & wave==2008
replace cuartiles2008=3 if q10==3 & pais==15 & wave==2008
replace cuartiles2008=4 if q10>=4  & pais==15 & wave==2008
*2010
replace cuartiles2010=1 if q10<=1 & pais==15 & wave==2010
replace cuartiles2010=2 if q10==2 & pais==15 & wave==2010
replace cuartiles2010=3 if q10==3 & pais==15 & wave==2010
replace cuartiles2010=4 if q10>=4  & pais==15 & wave==2010
*2012
replace cuartiles2012=1 if q10new<=8 & pais==15 & wave==2012
replace cuartiles2012=2 if (q10new>=9 & q10new<=12) & pais==15 & wave==2012
replace cuartiles2012=3 if (q10new>=13 & q10new<=15) & pais==15 & wave==2012
replace cuartiles2012=4 if q10new==16  & pais==15 & wave==2012

*Venezuela

*2008
replace cuartiles2008=1 if q10<=2 & pais==16 & wave==2008
replace cuartiles2008=2 if q10==3 & pais==16 & wave==2008
replace cuartiles2008=3 if q10==4 & pais==16 & wave==2008
replace cuartiles2008=4 if q10>=5  & pais==16 & wave==2008*/

**Agrupar cuartiles
gen ingreso4=.
replace ingreso4=1 if cuartiles2008==1 | cuartiles2010==1 | cuartiles2012==1 | cuartiles2014==1
replace ingreso4=2 if cuartiles2008==2 | cuartiles2010==2 | cuartiles2012==2 | cuartiles2014==2
replace ingreso4=3 if cuartiles2008==3 | cuartiles2010==3 | cuartiles2012==3 | cuartiles2014==3
replace ingreso4=4 if cuartiles2008==4 | cuartiles2010==4 | cuartiles2012==4 | cuartiles2014==4
label define ingreso4 1"Ingreso I" 2"Ingreso II" 3"Ingreso III" 4"Ingreso IV"
label values ingreso4 ingreso4


****Terciles
egen terciles2008 = xtile (q10), by (pais), if wave==2008, nquantiles(3)
egen terciles2010 = xtile (q10), by (pais), if wave==2010, nquantiles(3)
egen terciles2012 = xtile (q10new), by (pais), if wave==2012, nquantiles(3)
egen terciles2014 = xtile (q10new), by (pais), if wave==2014, nquantiles(3)

set more off

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 terciles2008 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10 terciles2010 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new terciles2012 if group == `i'
}

su group, meanonly
forvalues i = 1/`r(max)' {
tab q10new terciles2014 if group == `i'
}

**Agrupar terciles
gen ingreso3=.
replace ingreso3=1 if terciles2008==1 | terciles2010==1 | terciles2012==1 | terciles2014==1
replace ingreso3=2 if terciles2008==2 | terciles2010==2 | terciles2012==2 | terciles2014==2
replace ingreso3=3 if terciles2008==3 | terciles2010==3 | terciles2012==3 | terciles2014==3
label define ingreso3 1"Ingreso I" 2"Ingreso II" 3"Ingreso III"
label values ingreso3 ingreso3

table pais ingreso3


**1.8. Urbano
gen urbano=.
replace urbano=0 if ur==2
replace urbano=1 if ur==1
*label define urbano 0"Rural" 1"Urbano"
*label values urbano urbano

**1.9. Tamaño Ciudad
rename tamano tamano1
generat tamano = tamano1
replace tamano = 1 + 5 - tamano1
replace tamano = . if tamano1<1
label define tamano 1"Área rural" 2"Ciudad pequeña" 3"Ciudad mediana" 4"Ciudad grande" 5"Capital nacional"
label values tamano tamano

*1.10. Confianza en el sistema
rename b12 ffaa
rename b18 policia
rename b10a judicial
rename b21a ejecutivo
rename b13 congreso
rename b21 partidos

recode ffaa (miss=0), gen (ffaa1) 
recode policia (miss=0), gen (policia1)
recode judicial (miss=0), gen (judicial1)
recode ejecutivo (miss=0), gen (ejecutivo1)
recode congreso (miss=0), gen (congreso1)
recode partidos (miss=0), gen (partidos1)

egen resp_confianza = anycount(ffaa policia judicial ejecutivo congreso partidos), values(1/7)
gen tot_confianza = ffaa1 + policia1 + judicial1 + ejecutivo1 + congreso1 + partidos1
gen confianza = tot_confianza / resp_confianza
gen confianza1 = confianza-1


*************************** Tiempo ***************************

*2008
gen ano_2008=0
replace ano_2008=1 if wave==2008
*2010
gen ano_2010=0
replace ano_2010=1 if wave==2010
*2012
gen ano_2012=0
replace ano_2012=1 if wave==2012
*2014
gen ano_2014=0
replace ano_2014=1 if wave==2014

save "LAPOP_N1_Completa.dta", replace


*************************** Limpieza ***************************

use "LAPOP_N1_Completa.dta", clear

order pais wave idnum pais_r pais_año1 weight1500 wt ros4 ros4_r ingreso3 ingreso4 ingreso5 hombre edad ///
casado izquierda idpolitica sitlaboral educacion urbano tamano confianza ///
ano_2008 ano_2010 ano_2012 ano_2014

keep pais wave idnum pais_r pais_año1 weight1500 wt ros4 ros4_r ingreso3 ingreso4 ingreso5 hombre edad ///
casado izquierda idpolitica sitlaboral educacion urbano tamano confianza ///
ano_2008 ano_2010 ano_2012 ano_2014

save "LAPOP_N1_Filtrada.dta", replace


*************************** Nivel 2 ***************************

*cd "C:\Users\Gonzalo\Dropbox\Magister\Tesis\170930 Trabajo\Bases Finales"
use "LAPOP_N1_Filtrada.dta", clear
merge m:1 pais wave using "Franetovic_N2.dta", gen (merge_nivel2)

drop merge_nivel2

encode bienestar, gen (bienestar_r)
drop bienestar
rename bienestar_r bienestar

encode bienestar1, gen (bienestar_r1)
drop bienestar1
rename bienestar_r1 bienestar1

rename pais_año1 pais_ano1
sort pais_ano1
by pais_ano1: gen id_paisano = _n 

****Reorden países
recode pais (17 = 1) (10 = 2) (15 = 3) (13 = 4) (8 = 5) (6 = 6) (9 = 7) ///
(3 = 8) (2 = 9) (4 = 10) (1 = 11) (5 = 12) (7 = 13) /// 
(12 = 14) (11 = 15) (18 = 16) (14 = 17) (16 = 18), gen (pais123)
drop pais
label drop pais
rename pais123 pais

label define pais 1"Argentina" ///
2"Bolivia" ///
3"Brasil" ///
4"Chile" ///
5"Colombia" ///
6"Costa Rica" ///
7"Ecuador" ///
8"El Salvador" ///
9"Guatemala" ///
10"Honduras" ///
11"Mexico" ///
12"Nicaragua" ///
13"Panama" ///
14"Paraguay" ///
15"Peru" ///
16"Rep Dominicana" ///
17"Uruguay" ///
18"Venezuela"
label values pais pais

recode pais_r (17 = 1) (10 = 2) (15 = 3) (13 = 4) (8 = 5) (6 = 6) (9 = 7) ///
(3 = 8) (2 = 9) (4 = 10) (1 = 11) (5 = 12) (7 = 13) /// 
(12 = 14) (11 = 15) (18 = 16) (14 = 17) (16 = 18), gen (pais123_r)
drop pais_r
label drop pais_r
rename pais123_r pais_r

label define pais_r 1"ARG" ///
2"BOL" ///
3"BRA" ///
4"CHL" ///
5"COL" ///
6"CRI" ///
7"ECU" ///
8"SLV" ///
9"GTM" ///
10"HND" ///
11"MEX" ///
12"NIC" ///
13"PAN" ///
14"PRY" ///
15"PER" ///
16"DOM" ///
17"URY" ///
18"VEN"
label values pais_r pais_r

rename pais_ano1 pais_ano
order pais pais_r wave pais_ano

*Confianza en el sistema político
sort pais_ano
by pais_ano: egen confianza_paisano=mean(confianza)
sort pais
by pais: egen confianza_mean=mean(confianza_paisano)
gen confianza_dif = confianza_paisano - confianza_mean

*Transformaciones experimentales (edad en décadas e ingreso cuadrático)
gen edad10=edad/10
gen ingreso3_2=ingreso3^2
gen ingreso4_2=ingreso4^2
gen ingreso5_2=ingreso5^2

save "Franetovic_N1N2_.dta", replace
saveold "Franetovic_N1N2_old_.dta", version(12) replace


*************************** Listwise ***************************

gen listwise = !missing(ros4, hombre, edad, casado, izquierda, ///
sitlaboral, educacion, ingreso5, urbano, confianza)

drop if listwise==0

save "Franetovic_N1N2_listwise.dta", replace
saveold "Franetovic_N1N2_old_listwise.dta", version(12) replace


*************************** MODELOS HÍBRIDOS ***************************

*************************** (SCRIPT EN R) ***************************

/*mixed c.ros4 c.ingreso i.hombre c.edad10 i.casado i.idpolitica c.confianza i.sitlaboral i.educacion i.urbano ///
 c.ingreso#c.confianza c.ingreso2#c.confianza i.wave i.bienestar c.gsocial ///
 c.gini_dif c.gini_mean c.confianza_dif c.confianza_mean c.ingreso#c.confianza_dif c.ingreso#c.confianza_mean ///
 c.ingreso2#c.confianza_dif c.ingreso2#c.confianza_mean || pais: c.ingreso|| pais_ano: c.ingreso, mle var

mixed c.ros4 c.ingreso i.hombre c.edad10 i.casado i.idpolitica c.confianza i.sitlaboral i.educacion i.urbano ///
 c.ingreso#c.confianza c.ingreso2#c.confianza i.wave i.bienestar c.gsocial ///
 c.gini_dif c.gini_mean c.confianza_dif c.confianza_mean c.ingreso#c.gini_dif c.ingreso#c.gini_mean ///
 c.ingreso2#c.gini_dif c.ingreso2#c.gini_mean || pais: c.ingreso|| pais_ano: c.ingreso, mle var

mixed c.ros4 c.ingreso i.hombre c.edad10 i.casado i.idpolitica c.confianza i.sitlaboral i.educacion i.urbano ///
 c.ingreso#c.confianza c.ingreso2#c.confianza i.wave i.bienestar c.gsocial ///
 c.gini_dif c.gini_mean c.confianza_dif c.confianza_mean c.ingreso#i.bienestar c.ingreso2#i.bienestar || pais: c.ingreso|| pais_ano: c.ingreso, mle var

mixed c.ros4 i.ingreso i.hombre c.edad10 i.casado i.idpolitica c.confianza i.sitlaboral i.educacion i.urbano ///
 i.ingreso#c.confianza i.wave i.bienestar c.gsocial ///
 c.gini_mean c.gini_dif c.informal_mean c.informal_dif i.ingreso#c.gini_mean i.ingreso#c.gini_dif ///
 i.ingreso#c.informal_mean i.ingreso#c.informal_dif || pais: R.ingreso|| pais_ano: R.ingreso, mle var

mixed c.ros4 i.ingreso i.hombre c.edad10 i.casado i.idpolitica c.confianza i.sitlaboral i.educacion i.urbano ///
 i.ingreso#c.confianza i.wave i.bienestar c.gsocial ///
 c.gini_mean c.gini_dif c.pib_mean c.pib_dif i.ingreso#c.gini_mean i.ingreso#c.gini_dif ///
 i.ingreso#c.pib_mean i.ingreso#c.pib_dif || pais: R.ingreso|| pais_ano: R.ingreso, mle var*/
 
 
 
