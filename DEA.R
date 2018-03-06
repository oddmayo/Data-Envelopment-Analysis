####################################
## ANÁLISIS DE ENVOLTURA DE DATOS ##
####################################

library(readxl) 
library(Benchmarking) #paquete del libro guía

#######################################################
# JUZGADOS proceso reparación directa entre CIRCUITOS #
#######################################################
juzgcirc <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 1)

inputs1 <- data.frame(c(juzgcirc[5],juzgcirc[6])) #vector de entradas
inputs1 <- as.matrix(inputs1) #matriz necesaria para la función dea de Benchmarking
outputs1 <- data.frame(juzgcirc[3]) #vector de salidas
outputs1 <- as.matrix(outputs1)
dea1 <- dea(inputs1,outputs1,RTS="vrs",ORIENTATION = "out",SLACK = TRUE, DUAL = TRUE) #rendimientos variables, orientado a salidas y 2 etapas
# eficiencia técnica
eftcircj <- 1/dea1$eff 
eftcircj
eft_circj <- data.frame(Circuito=juzgcirc$CIRCUITO,Eficiencia_técnica=eftcircj)
eft_circj
# unidades de referencia
unidades1 <- data.frame(dea1$lambda)
# recomendación
recomen1 <- data.frame(Circuito=juzgcirc$CIRCUITO,Bogotá=unidades1$L5,
                      Leticia=unidades1$L16,
                      Mocoa=unidades1$L19,
                      Pamplona=unidades1$L22,
                      Pasto=unidades1$L23,
                      Eficiencia_técnica=eftcircj,
                      "Aumento_recomendado_en_porcentaje"=(1-eftcircj)*100)
recomen1
# holguras (eficiencia fuerte o débil)
dea1$sx
dea1$sy
# pesos (sensibilidad/impacto sobre la eficiencia)
dea1$ux
dea1$vy


# supereficiencia (Para detectar valores atípico/Diferenciar unidades frontera)
superdea1 <- sdea(inputs1,outputs1,RTS = "vrs",ORIENTATION = "out")
1/superdea1$eff
superef1 <- data.frame(Circuito=juzgcirc$CIRCUITO,Super_eficiencia=1/superdea1$eff)
superef1

superdea1$lambda


#######################################################
# JUZGADOS proceso reparación directa entre DISTRITOS #
#######################################################
juzgdist <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 4)

inputs2 <- data.frame(c(juzgdist[5],juzgdist[6])) #vector de entradas
inputs2 <- as.matrix(inputs2) #matriz necesaria para la función dea de Benchmarking
outputs2 <- data.frame(juzgdist[3]) #vector de salidas
outputs2 <- as.matrix(outputs2)
dea2 <- dea(inputs2,outputs2,RTS="vrs",ORIENTATION = "out",SLACK = TRUE, DUAL = TRUE) #rendimientos variables, orientado a salidas y 2 etapas
# eficiencia técnica
eftdistj <- 1/dea2$eff 
eftdistj
eft_distj <- data.frame(Circuito=juzgdist$DISTRITO,Eficiencia_técnica=eftdistj)
eft_distj
# unidades de referencia
dea2$lambda
# holguras (eficiencia fuerte o débil)
dea2$sx
dea2$sy

# supereficiencia (para detectar valores atípico/diferenciar unidades frontera)
superdea2 <- sdea(inputs2,outputs2,RTS = "vrs",ORIENTATION = "out")
1/superdea2$eff
superef2 <- data.frame(Distrito=juzgdist$DISTRITO,Super_eficiencia=1/superdea2$eff)
superef2

superdea2$lambda



#########################################################
# TRIBUNALES proceso reparación directa entre DISTRITOS #
#########################################################
tribdist <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 6)

inputs3 <- data.frame(c(tribdist[5],tribdist[6])) #vector de entradas
inputs3 <- as.matrix(inputs3) #matriz necesaria para la función dea de Benchmarking
outputs3 <- data.frame(tribdist[3]) #vector de salidas
outputs3 <- as.matrix(outputs3)
dea3 <- dea(inputs3,outputs3,RTS="vrs",ORIENTATION = "out",SLACK = TRUE, DUAL = TRUE) #rendimientos variables, orientado a salidas y 2 etapas
# eficiencia técnica
eftdistt <- 1/dea3$eff 
eftdistt
eft_distt <- data.frame(Distrito=tribdist$DISTRITO,Eficiencia_técnica=eftdistt)
eft_distt
# unidades de referencia
unidades3 <- data.frame(dea3$lambda)
# recomendación
recomen3 <- data.frame(Distrito=tribdist$DISTRITO,Arauca=unidades3$L2,
                       Cundinamarca=unidades3$L13,
                       San_Andrés=unidades3$L22,
                       Pamplona=unidades3$L22,
                       Eficiencia_técnica=eftdistt,
                       "Aumento_recomendado_en_porcentaje"=(1-eftdistt)*100)
recomen3
# holguras (evidencia eficiencia fuerte o débil)
dea3$sx
dea3$sy
# supereficiencia (para detectar valores atípico/diferenciar unidades frontera)
superdea3 <- sdea(inputs3,outputs3,RTS = "vrs",ORIENTATION = "out")
1/superdea3$eff
superef3 <- data.frame(Distrito=tribdist$DISTRITO,Super_eficiencia=1/superdea3$eff)
superef3

superdea3$lambda


##########################
## EXPORTAR eficiencias ##
##########################
library(xlsx)
write.xlsx(eft_circj,"C:/Users/CamiloAndrés/Desktop/DNP/basejc.xlsx")
write.xlsx(eft_distj,"C:/Users/CamiloAndrés/Desktop/DNP/basejd.xlsx")
write.xlsx(eft_distt,"C:/Users/CamiloAndrés/Desktop/DNP/basetd.xlsx")

###########################################################################
## GRÁFICOS DE FRONTERAS DE PRODUCCIÓN (2 inputs para producir 1 output) ##
###########################################################################
library(ggplot2)
library(ggrepel)

#########################################
# Gráfico para JUZGADOS entre CIRCUITOS #
#########################################

########### Gráfico con función del paquete para saber forma de la frontera
inputnj_1 <- as.matrix(inputnj_1)
inputct_1 <- as.matrix(inputct_1)
plotef <- dea.plot.frontier(inputnj_1,inputct_1,RTS = "vrs")
###########

tablaf1 <- data.frame(juzgcirc,eficiencia=dea1$eff)
myColors <- c("firebrick4", "slateblue4")
tablalabelf1 <- data.frame(juzgcirc,eficiencia=dea1$eff)
tablalabelf1$eficiencia[-which(tablalabelf1$eficiencia==1)]=0
tablalabelf1$color=rep("firebrick4",length(tablalabelf1$CIRCUITO))
tablalabelf1$color[which(tablalabelf1$eficiencia==1)]=rep("slateblue4", 5)
tablalabelf1$eficiencia = as.factor(tablalabelf1$eficiencia)

tablaff1 <- tablaf1[which(tablaf1$eficiencia==1),] 
tablaff1

fronteraJC <- ggplot(data=tablalabelf1,
                     aes(x=Jueces,
                         y=Carga,
                         label=CIRCUITO)
                     )+
              geom_line(data=tablaff1,
                        aes(x=Jueces,
                            y=Carga),
                        color="firebrick4",
                        cex=1,
                        linetype="F1"
                        )+
              geom_point(aes(color=eficiencia,
                             size=eficiencia)
                         )+
              scale_color_manual(values=myColors
                                 )+
              theme(legend.position = "none",
                    rect=element_rect(fill = "transparent"),
                    plot.title = element_text(hjust = 0.5),
                    # para fondo blanco
                    panel.grid=element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black")
                    )+
              
              ggtitle("Eficiencia técnica proceso reparación directa entre circuito (juzgados)"
                      )+
              labs(x="Número de jueces",
                   y="Carga de trabajo (Demanda+Inventario)"
                   )+
              geom_label_repel(aes(label=CIRCUITO,
                                   color=eficiencia),
                               force=8,
                               arrow = arrow(length = unit(0.5, 'picas'))
                              )
fronteraJC



###########################################
# Gráfico para TRIBUNALES entre DISTRITOS #
###########################################

########### Gráfico con función del paquete para saber forma de la frontera
input_3 <- as.matrix(input_3)
output_3 <- as.matrix(output_3)
plotef3 <- dea.plot.frontier(input_3,output_3,RTS = "vrs")
###########

tablaf3 <- data.frame(tribdist,eficiencia=dea3$eff)
tablalabelf3 <- data.frame(tribdist,eficiencia=dea3$eff)
tablalabelf3$eficiencia[-which(tablalabelf3$eficiencia==1)]=0  
tablalabelf3$color=rep("firebrick4",length(tablalabelf3$DISTRITO))
tablalabelf3$color[which(tablalabelf3$eficiencia==1)]=rep("slateblue4",3)
tablalabelf3$eficiencia = as.factor(tablalabelf3$eficiencia)

tablaff3 <- tablaf3[which(tablaf3$eficiencia==1),] 


fronteraTD <- ggplot(data=tablalabelf3,
                     aes(x=Jueces,
                         y=Carga,
                         label=DISTRITO)
                    )+
              geom_line(data=tablaff3,
                        aes(x=Jueces,
                            y=Carga),
                        color="firebrick4",
                        cex=1,
                        linetype="F1"
                        )+
              geom_point(aes(color=eficiencia,
                             size=eficiencia)
                        )+
              scale_color_manual(values=myColors
                                )+
              theme(legend.position = "none",
                    rect=element_rect(fill = "transparent"),
                    plot.title = element_text(hjust = 0.5)
                    )+
              ggtitle("Eficiencia técnica proceso reparación directa entre distritos para tribunales"
                      )+
              labs(x="Número de jueces",
                  y="Carga de trabajo (Demanda+Inventario)"
                  )+
              geom_label_repel(aes(label=DISTRITO,
                                   color=eficiencia),
                               force=8,
                               arrow = arrow(length = unit(0.5, 'picas'))
                              )
fronteraTD

######################
##### DATA CLOUD #####
######################

# Datos atípicos para juzgados entre circuit
x1 <- with(juzgcirc, cbind(juzgcirc$Jueces))
y1 <- with(juzgcirc, cbind(juzgcirc$Carga))
w1 <- with(juzgcirc, cbind(juzgcirc$Egresos))
x1y1w1 <- cbind(x1,y1,w1)
D1 <- det(t(x1y1w1)%*%x1y1w1)
i1 <- c(1) # DMUs a quitarse, que se sospeche que sean atípicos
x1y1w1i1 = x1y1w1[-i1,]
D1i <- det( t(x1y1w1i1) %*% x1y1w1i1 )
R1i <- D1i/D1
R1i # Valores pequeños de Ri me indican presencia de datos atípicos

# Datos atípicos para juzgados entre distritos
x2 <- with(juzgdist, cbind(juzgdist$Jueces))
y2 <- with(juzgdist, cbind(juzgdist$Carga))
w2 <- with(juzgdist, cbind(juzgdist$Egresos))
x2y2w2 <- cbind(x2,y2,w2)
D2 <- det(t(x2y2w2)%*%x2y2w2)
i2 <- c(1) # DMUs a quitarse, que se sospeche que sean atípicos
x2y2w2i2 = x2y2w2[-i2,]
D2i <- det( t(x2y2w2i2) %*% x2y2w2i2 )
R2i <- D2i/D2
R2i # Valores pequeños de Ri me indican presencia de datos atípicos

# Datos atípicos para tribunales entre distritos
x3 <- with(tribdist, cbind(tribdist$Jueces))
y3 <- with(tribdist, cbind(tribdist$Carga))
w3 <- with(tribdist, cbind(tribdist$Egresos))
x3y3w3 <- cbind(x3,y3,w3)
D3 <- det(t(x3y3w3)%*%x3y3w3)
i3 <- c(1) # DMUs a quitarse, que se sospeche que sean atípicos
x3y3w3i3 = x3y3w3[-i3,]
D3i <- det( t(x3y3w3i3) %*% x3y3w3i3 )
R3i <- D3i/D3
R3i # Valores pequeños de Ri me indican presencia de datos atípicos

