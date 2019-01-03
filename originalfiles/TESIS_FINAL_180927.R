################### ENTORNO Y PAQUETES ################### ----
pacman::p_load(foreign,
               stargazer,
               xtable,
               psych,
               psy,
               base, 
               magrittr,
               readxl,
               lme4,
               texreg,
               varhandle,
               countrycode,
               car,
               laeken, 
               multilevel,
               ggplot2,
               ggthemes,
               extrafont,
               plyr,
               data.table,
               dplyr,
               doBy,
               multiwayvcov,
               miceadds,
               RColorBrewer,
               scales,
               haven,
               margins,
               grDevices,
               interplot,
               lmtest,
               lsmeans,
               lattice,
               broom,
               sjPlot,
               lmerTest,
               labelled)


################### ORIGINAL ###################
#setwd("C:/Users/Gonzalo/Documents/Magister Respaldos/Tesis Respaldos/Bases Madres")
#load("LAPOPFULL.RData") 
#setwd("C:/Users/Gonzalo/Documents/Magister Respaldos/Tesis Respaldos/Bases Originales")
#write.csv(lapop, file = "COMPLETA1_Lapop2.csv",row.names=FALSE, na="", dec=".", sep=";")


################### LAPOP ###################

lapop1 = read.dta("Franetovic_N1N2_old_.dta")
lapop_pais = read.dta("Franetovic_N2_old_.dta")


#Listwise variables N1
lapop = lapop1[complete.cases(lapop1$ros4, lapop1$hombre, lapop1$edad, lapop1$casado, 
                              lapop1$izquierda, lapop1$idpolitica, lapop1$sitlaboral, 
                              lapop1$educacion, lapop1$ingreso5, lapop1$urbano,
                              lapop1$confianza), ]
rm(lapop1)

#Cambio de tipo de variable
lapop$ros4 = as.integer(lapop$ros4)
lapop$wave1 = as.factor(lapop$wave)
lapop$ingreso5_r=as.integer(lapop$ingreso5)
lapop$ingreso4_r=as.integer(lapop$ingreso4)
lapop$ingreso3_r=as.integer(lapop$ingreso3)
lapop$hombre_r=as.integer(lapop$hombre)

sapply(lapop, class)
attach(lapop)


######################## TABLAS Y FIGURAS  ####################### ----


###TABLA 1: ESTAD?STICOS DESCRIPTIVOS ----

##Nivel 1
redistribucion_n = as.numeric(lapop$ros4)
ingreso5_n = as.numeric(lapop$ingreso5)
sitlaboral_n = as.numeric(lapop$sitlaboral)
educacion_n = as.numeric(lapop$educacion)
izquierda_n = as.numeric(lapop$izquierda)
confianza_n = as.numeric(lapop$confianza)
hombre_n = as.numeric(lapop$hombre)
edad_n = as.numeric(lapop$edad)
casado_n = as.numeric(as.character(lapop$casado))
urbano_n = as.numeric(lapop$urbano)

nivel1 = as.data.frame(cbind(redistribucion_n, 
                             ingreso5_n, sitlaboral_n, educacion_n, 
                             izquierda_n, confianza_n,
                             hombre_n, edad_n, casado_n, urbano_n))
colnames(nivel1) <- c("Acuerdo con redistribuci?n", 
                      "Ingreso", "Situaci?n laboral", "Educaci?n", 
                      "Izquierda", "Confianza en el sistema",
                      "Hombre", "Edad", "Casado", "Urbano" )

stargazer(nivel1)

#Correlaciones
cor2latex(nivel1, stars=TRUE)


##Nivel 2
lapop_pais = aggregate(lapop, by=list(lapop$pais, lapop$wave), FUN=mean, na.rm=TRUE) 

holahola = as.character(lapop_pais$Group.1)

nivel2a = as.data.frame(cbind(holahola, lapop_pais$Group.2, 
                             lapop_pais$gini_mean, lapop_pais$gini_dif,
                             lapop_pais$pib_mean, lapop_pais$pib_dif))

colnames(nivel2a) <- c("Pa?s", "A?o", "Gini [BE]", "Gini [WE]",
                      "P?B [BE]", "PIB [WE]")

nivel2 = as.data.frame(cbind(lapop_pais$gini_mean, lapop_pais$gini_dif,
                              lapop_pais$pib_mean, lapop_pais$pib_dif))

colnames(nivel2) <- c("Gini [BE]", "Gini [WE]",
                       "P?B [BE]", "PIB [WE]")

stargazer(nivel2)

#Correlaciones
cor2latex(nivel2, stars=TRUE)



###FIGURA 2: ROS4 / PA?SES ----

charts.data1 <- read.csv("G1.csv", sep=";")
colnames(charts.data1) <- c("pais1", "pais", "porcen", "ros4", "orden")
sapply(charts.data1, class)
charts.data1$ros4 = as.factor(charts.data1$ros4)
charts.data1$ros4 <- factor(charts.data1$ros4, levels = c("7","6","5","4","3","2","1"))
charts.data1$ros4 <- factor(charts.data1$ros4, levels = c("1","2","3","4","5","6","7"))
charts.data1 = (charts.data1, .(orden),
                    transform, pos = cumsum(porcen) - (0.5 * porcen))
charts.data1$pos1= 100 - charts.data1$pos

g1 = ggplot(data = charts.data1, aes(x=reorder(pais, orden), y = porcen, fill = factor(ros4))) + 
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "", fill="") +
  scale_fill_grey(start = .9, end = .25, name=" ") +
  scale_y_continuous(labels = function(porcen){ paste0(porcen, "%") }) +
  theme(panel.background = element_rect(fill = "white"), 
        axis.text=element_text(size=10),
        strip.text=element_text(size=12))
g1


g1c = ggplot(data = charts.data1, aes(x=reorder(pais1, orden), y = porcen, fill = factor(ros4))) + 
  geom_bar(stat = "identity") + coord_flip() + 
  labs(x = "", y = "", fill="") +
  scale_fill_brewer(palette="Blues") +
  scale_y_continuous(labels = function(porcen){ paste0(porcen, "%") }) +
  theme(panel.background = element_rect(fill = "white"), 
        axis.text=element_text(size=10),
        strip.text=element_text(size=12))
g1c

###FIGURA 3: ROS4 / PAISES / A?OS ----

lapop$ros4a=as.integer(lapop$ros4)

g2 = ggplot(lapop, aes(x= ros4a,  colour=wave1)) + 
  geom_line(aes(y = ..prop.., fill = factor(..x..)), stat="count", binwidth = 1, size = 1) +
  facet_wrap(~pais, ncol=3) +
  xlim(1, 7) +
  labs(x = "Acuerdo con Redistribuci?n", y = "", fill="") +
  scale_color_grey(start=0.8, end=0.2, name="A?o") +  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1:7)) +
  theme(axis.text=element_text(size=15),
        strip.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size=unit(1.5,"cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  theme_hc()
g2


g2c= ggplot(lapop, aes(x= ros4a,  colour=wave1)) + 
  geom_freqpoly(aes(y = ..prop.., fill = factor(..x..)), stat="count", binwidth = 1, size = 1) +
  xlim(1, 7) +
  labs(x = "Acuerdo con Redistribuci?n", y = "", fill="") +
  facet_wrap(~pais, ncol=3) +
  scale_colour_brewer(name = "A?o", palette="Spectral") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1:7)) +
  theme(axis.text=element_text(size=15),
        strip.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size=unit(1.5,"cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  theme_hc()
g2c


###TABLA 2: ROS4 / INGRESO ----

#Promedios por Pa?s/A?o: Ros4, Gini, Ingreso
ros4_pais=as.data.frame(tapply(lapop$ros4, lapop$pais, mean))
ros4_paisano = with(lapop, tapply(ros4, list(pais, wave1), mean))

ros4_ingreso_paisano = with(lapop, tapply(ros4, list(pais, ingreso5), mean))
ros4_ipaisano = with(lapop, tapply(ros4, list(pais, ingreso5), mean))
ros4_ingreso_total = with(lapop, tapply(ros4, list(ingreso5), mean))

gini_paisano = with(lapop, tapply(gini, list(pais, wave1), mean))
pib_paisano = with(lapop, tapply(pib, list(pais, wave1), mean))

#Descriptivo: Ros4 - Ingreso - Paises
matriz_ingreso_ros4 = as.data.frame(ros4_ingreso_paisano)
matriz_ingreso_ros4$pais = rownames(matriz_ingreso_ros4)

ingreso5_r_paisano2=reshape(matriz_ingreso_ros4, direction="long", varying=list(names(matriz_ingreso_ros4)[1:5]), v.names="ros4", 
                            idvar=c("pais"), timevar="quintil", times=1:5)

ingreso5_r_paisano2$pais=as.factor(ingreso5_r_paisano2$pais)

#Tabla 2
ros4_ingreso_paisano
ros4_pais
ros4_ingreso_total








######################## ESTIMACI?N MULTINIVEL #######################----

sapply(lapop, class)

###0. MODELO NULO - ICC

#Pais a?o (SIN FE A?O)
modelo_0 = lmer(ros4 ~ 1 + (1 | pais_ano) + (1 | pais)) 
summary(modelo_0)
varcomp=as.data.frame(VarCorr(modelo_0))
varcomp
o2pais=varcomp[2,4] 
o2paisano=varcomp[1,4]
o2individuo=varcomp[3,4] 
icc_pais=o2pais/(o2pais+o2paisano+o2individuo)
icc_paisano=o2paisano/(o2pais+o2paisano+o2individuo)
icc_individuo=o2individuo/(o2pais+o2paisano+o2individuo)

icc_pais #0.04127628
icc_paisano #0.03680775
icc_individuo #0.921916


# Ingreso continua
lapop$ingreso5_r2=lapop$ingreso5_r^2
attach(lapop)


###1. MODELO INGRESO

modelo_1 = lmer(ros4 ~ 1 + ingreso5_r
                + wave1 
                + (1 | pais_ano) + (1 | pais))

###2. MODELO INDIVIDUAL (H4)

modelo_2 = lmer(ros4 ~ 1 + ingreso5_r  + hombre + edad 
                 + casado + izquierda + confianza + sitlaboral 
                 + educacion + urbano + wave1
                 + (1 | pais_ano) + (1 | pais), data=lapop)


###3. MODELO GINI (H1 Y H2)

modelo_3 = lmer(ros4 ~ 1 + ingreso5_r  + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_mean + gini_dif
                + (1 | pais_ano) + (1 | pais))

###4. MODELO GINI + PIB

modelo_4 = lmer(ros4 ~ 1 + ingreso5_r + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_mean + gini_dif
                + pib_mean + pib_dif
                + (1 | pais_ano) + (1 | pais))

###5. MODELO GINI + PIB + BIENESTAR (H3)

modelo_5 = lmer(ros4 ~ 1 + ingreso5_r + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_mean + gini_dif
                + pib_mean + pib_dif
                + bienestar
                + (1 | pais_ano) + (1 | pais))

###6. MODELO PAIS, PAISANO Y INDIVIDUAL (INGRESO ALEATORIO) (H4.2)

modelo_6 = lmer(ros4 ~ 1 + ingreso5_r + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_dif + gini_mean 
                + pib_dif + pib_mean
                + (1 + ingreso5_r | pais_ano) + (1 + ingreso5_r | pais))

###7. MODELO PAIS, PAISANO Y INDIVIDUAL (INGRESO ALEATORIO) (INGRESO*GINI) (H5)

modelo_7 = lmer(ros4 ~ 1 + ingreso5_r + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_mean + gini_dif
                + pib_mean + pib_dif
                + ingreso5_r*gini_mean + ingreso5_r*gini_dif
                + (1 + ingreso5_r | pais_ano) + (1 + ingreso5_r | pais))

###8. MODELO PAIS, PAISANO Y INDIVIDUAL (INGRESO ALEATORIO) (INGRESO*PIB)

modelo_8 = lmer(ros4 ~ 1 + ingreso5_r + hombre + edad 
                + casado + izquierda + confianza + sitlaboral 
                + educacion + urbano + wave1
                + gini_mean + gini_dif
                + pib_mean + pib_dif
                + ingreso5_r*pib_mean + ingreso5_r*pib_dif
                + (1 + ingreso5_r | pais_ano) + (1 + ingreso5_r | pais))

#Revisar comando con los ponderadores waves. 

texreg(list(modelo_0,modelo_1, modelo_2, modelo_3, modelo_4, 
            modelo_5, modelo_6, modelo_7, modelo_8), 
       stars = c(0.01,0.05,0.1), dcolumn = TRUE)


#LR Test (Test Modelos Anidados)
lrtest (modelo_0, modelo_1)
lrtest (modelo_1, modelo_2)
lrtest (modelo_2, modelo_3)
lrtest (modelo_3, modelo_4)
lrtest (modelo_4, modelo_5)
lrtest (modelo_4, modelo_6)
lrtest (modelo_6, modelo_7)
lrtest (modelo_7, modelo_8)

#Gr?fico

coef_mod = as.data.frame(tidy(modelo_1, conf.int = TRUE))
coef

ggplot(coef_mod, aes(term, estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  labs(title = "Coeficientes de un modelo de regresi?n OLS")

cplot_modelo1 = coefplot(modelo_1)


###FIGURA 4: EFECTO ALEATORIO INGRESO ----

#Intercepto
plot_model(modelo_6, 
           type = "re", 
           show.legend = TRUE,
           show.values = TRUE,
           sort.est = "ingreso5_r",
           facet.grid = TRUE,
           y.offset = .4,
           value.offset = .4,
           value.size = 3.5,
           title="Efecto aleatorio de Ingreso sobre Acuerdo con Redistribuci?n por pa?s: intercepto y pendiente")

ranef(modelo_6)


###FIGURA 5: EFECTOS MARGINALES ----

#GINI

em_ing_ginibe1 = interplot(m = modelo_7, var1 = 'ingreso5_r', var2 = 'gini_mean', ci = 0.90) + 
  xlab("Gini (Promedio)") +
  ylab("Cambio en Acuerdo con Redistribuci?n") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
em_ing_ginibe1

em_ing_giniwe1 = interplot(m = modelo_7, var1 = 'ingreso5_r', var2 = 'gini_dif', ci = 0.90) + 
  xlab("Gini (Cambio)") +
  ylab("Cambio en Acuerdo con Redistribuci?n") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
em_ing_giniwe1


#PIB

em_ing_pibbe1 = interplot(m = modelo_8, var1 = 'ingreso5_r', var2 = 'pib_mean', ci = 0.90) + 
  xlab("PIB (Promedio)") +
  ylab("Cambio en Acuerdo con Redistribuci?n") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
em_ing_pibbe1

em_ing_pibwe1 = interplot(m = modelo_8, var1 = 'ingreso5_r', var2 = 'pib_dif', ci = 0.90) + 
  xlab("PIB (Cambio)") +
  ylab("Cambio en Acuerdo con Redistribuci?n") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text=element_text(size=11),
        axis.title.y = element_text(size =12),
        axis.title.x = element_text(size = 12))
em_ing_pibwe1

######################## ANEXOS ######################## ----

#ANEXO 1: N MUESTRA
lapop$casos=1
n_paisano = with(lapop, tapply(casos, list(pais, wave1), sum))
n_pais = as.data.frame(with(lapop, tapply(casos, list(pais), sum)))
n_ano = as.data.frame(with(lapop, tapply(casos, list(wave1), sum)))

xtable(n_paisano, digits = 0)
xtable(n_pais, digits = 0)
xtable(n_ano, digits = 0)


#ANEXO 2: ROS 4 / INGRESO / A?O / PA?SES

quintiles_ros4 = aggregate(lapop, by=list(lapop$pais, lapop$wave, lapop$ingreso5_r), FUN=mean, na.rm=TRUE) 
quintiles_ros4$pais = quintiles_ros4$Group.1
quintiles_ros4$ano = as.factor(quintiles_ros4$Group.2)
quintiles_ros4$ingreso = quintiles_ros4$Group.3


a2 = ggplot(quintiles_ros4, aes(x=ingreso, y=ros4, colour=ano)) + 
  geom_line(stat="identity", binwidth = 1, size = 1) +
  facet_wrap(~pais, ncol=3) +
  scale_colour_grey(start = .7, end = .25, name="A?o") +
  scale_y_continuous(breaks = c(1:7), limits = c(1,7))+
  labs(x = "Ingreso (Quintiles)", y = "Acuerdo con Redistribuci?n", fill="") +
  theme(axis.text=element_text(size=15),
        strip.text=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=15),
        legend.key.size=unit(1.5,"cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")) +
  theme_hc()
a2


#ANEXO 3: GINI Y PIB - WITHIN Y BETWEEN

##Between

m_between = aggregate(lapop, 
                      by=list(lapop$pais_r), FUN=mean, na.rm=TRUE) 

a3a= ggplot(m_between, 
            aes(x= gini_mean, y= ros4, label=Group.1)) +
  geom_point() + 
  geom_smooth(method = "lm", colour="black") +
  geom_text(aes(label=Group.1),hjust=-0.1, vjust=-0.5, size=3.5) +
  labs(x = "Desigualdad", y = "Acuerdo con Redistribuci?n", fill="") +
  scale_y_continuous("Acuerdo con Redistribuci?n", limits = c(4.8,6.2), 
                     breaks = c(4.8,5.2,5.6,6)) +
  scale_x_continuous("Desigualdad", limits = c(40,60),
                     breaks = c(40,50,60)) +    
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
a3a

a3b= ggplot(m_between, 
            aes(x= pib_mean, y= ros4, label=Group.1)) +
  geom_point() + 
  geom_smooth(method = "lm", colour="black") +
  geom_text(aes(label=Group.1),hjust=-0.1, vjust=-0.5, size=3.5) +
  labs(x = "Desarrollo Econ?mico", y = "Acuerdo con Redistribuci?n", fill="") +
  scale_y_continuous("Acuerdo con Redistribuci?n", limits = c(4.8,6.2), 
                     breaks = c(4.8,5.2,5.6,6)) +
  scale_x_continuous("Desarrollo Econ?mico", limits = c(1,14),
                     breaks = c(5,10,15)) +  
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
a3b


##Within

lapop$bienestarr=as.integer(lapop$bienestar)

lapop_paisano = aggregate(lapop, 
                          by=list(lapop$pais, lapop$pais_r, lapop$wave, 
                                  lapop$pais_ano), FUN=mean, na.rm=TRUE) 

lapop_paisano1 = select(lapop_paisano,
                         Group.1, Group.2, Group.3, Group.4,
                        ros4, gini, pib, confianza_paisano, 
                        gsocial, desempleo, informal, bienestarr)

colnames(lapop_paisano1) <- c("pais", "pais_r", "wave", "pais_ano", 
                              "ros4", "gini", "pib", "confianza",
                              "gsocial", "desempleo", "informal", "bienestar1")

a3c= ggplot(lapop_paisano1, 
            aes(x= gini, y= ros4, label=pais_ano, colour=pais)) +
  geom_point(size = 2, alpha = .8) + 
  geom_smooth(size = 0.8, method = "lm", se = FALSE) +
  geom_text(aes(label=pais_ano),hjust=0.5, vjust=-0.6, size=3) + 
  labs(x = "Desigualdad", y = "Acuerdo con Redistribuci?n") + 
  scale_y_continuous("Acuerdo con Redistribuci?n", limits = c(4.2,6.7), 
                     breaks = c(4.5,5,5.5,6,6.5)) +
  scale_x_continuous("Desigualdad",
                     breaks = c(40,50,60)) +  
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
a3c


a3d= ggplot(lapop_paisano1, 
            aes(x= pib, y= ros4, label=pais_ano, colour=pais)) +
  geom_point(size = 2, alpha = .8) + 
  geom_smooth(size = 0.8, method = "lm", se = FALSE) +
  geom_text(aes(label=pais_ano),hjust=0.5, vjust=-0.6, size=3) + 
  labs(x = "Desarrollo Econ?mico", y = "Acuerdo con Redistribuci?n") + 
  scale_y_continuous("Acuerdo con Redistribuci?n", limits = c(4.2,6.7), 
                     breaks = c(4.5,5,5.5,6,6.5)) +
  scale_x_continuous("Desarrollo Econ?mico", breaks = c(5,10)) +
  theme(panel.grid.major = element_line(colour = "grey"),
        legend.position="none",
        axis.text=element_text(size=10),
        strip.text=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12),
        legend.key.size=unit(1,"cm"),
        panel.background = element_rect(fill = "white"))
a3d

