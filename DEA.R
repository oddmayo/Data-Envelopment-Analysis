####################################
## ANÁLISIS DE ENVOLTURA DE DATOS ##
####################################

library(readxl)
##########################
######### DATOS ##########
##########################

juzgcirc <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 1)
juzgdist <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 4)
tribdist <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 6)

# lista de entradas y salidas
listainputnj  <- list(juzgcirc[5],juzgdist[5],tribdist[5])
listainputct <- list(juzgcirc[6],juzgdist[6],tribdist[6])
listaoutputs <- list(juzgcirc[3],juzgdist[3],tribdist[3])

# creación entradas para número de jueces
dataflist <- lapply(listainputnj, data.frame)
nam <- "inputnj_"
val <- c(1:length(dataflist))
for(i in 1:length(val)){
  assign(
    paste(nam, val, sep = "")[i], dataflist[[i]]
  ) }


# creación entradas para carga de trabajo
dataflist1.5 <- lapply(listainputct, data.frame)
nam1.5 <- "inputct_"
val1.5 <- c(1:length(dataflist1.5))
for (i in 1:length(val1.5)) {
  assign(
    paste(nam1.5, val1.5, sep = "")[i], dataflist1.5[[i]]
  )
}


# creación salidas
dataflist2 <- lapply(listaoutputs, data.frame)
nam2 <- "output_"
val2 <- c(1:length(dataflist2))
for(i in 1:length(val2)){
  assign(
    paste(nam2, val2, sep = "")[i], dataflist2[[i]]
  ) }

detach(package:Benchmarking)
library(rDEA)
# eficiencia técnica

X = juzgcirc[c('Jueces','Carga')]
Y=  juzgcirc[c('Egresos')]

firms=1:38

eftjcirc <- dea(XREF=X,YREF=Y,X=X[firms,],Y=Y[firms,],RTS = "variable",model = "input")
eftjdist <- dea(XREF=listainputs_2,YREF=output_2,X=listainputs_2,Y=output_2,RTS = "variable",model = "output")
efttdist <- dea(XREF=listainputs_3,YREF=output_3,X=listainputs_3,Y=output_3,RTS = "variable",model = "output")

ef_juzg_circ <- data.frame(Circuito=juzgcirc$CIRCUITO,Eficiencia_técnica=eftjcirc$thetaOpt)
ef_juzg_dist <- data.frame(Distrito=juzgdist$DISTRITO,Eficiencia_técnica=eftjdist$thetaOpt)
ef_trib_dist <- data.frame(Distrito=tribdist$DISTRITO,Eficiencia_técnica=efttdist$thetaOpt)

ef_juzg_circ
ef_juzg_dist
ef_trib_dist

eftjcirc$lambda




# exportar eficiencias
library(xlsx)
write.xlsx(ef_juzg_circ,"C:/Users/CamiloAndrés/Desktop/DNP/basejc.xlsx")
write.xlsx(ef_juzg_dist,"C:/Users/CamiloAndrés/Desktop/DNP/basejd.xlsx")
write.xlsx(ef_trib_dist,"C:/Users/CamiloAndrés/Desktop/DNP/basetd.xlsx")

# peers units de benchmark
detach(package:rDEA)
library(Benchmarking)

X <- data.frame(X)
Y <- data.frame(Y)
bench <- dea(X,Y,RTS="vrs",ORIENTATION = "out")


peerscir_j <- dea(input_1,output_1,RTS="vrs",ORIENTATION = "out",SLACK = TRUE)
peerscir_j$lambda
peersdis_j <- dea(input_2,output_2,RTS="vrs",ORIENTATION = "out",SLACK = TRUE)
peersdis_t <- dea(input_3,output_3,RTS="vrs",ORIENTATION = "out",SLACK = TRUE)


detach(package:rDEA)
library(Benchmarking)
library(ggrepel)
library(ggplot2)

# Gráfico de curvas de indiferencia/isocuantas


plot(juzgcirc$Jueces,juzgcirc$Carga)

textxy(juzgcirc$Jueces,juzgcirc$Carga,juzgcirc$CIRCUITO)


# GRáFICO frontera de producción (2 inputs para producir 1 output)


# Gráfico para juzgados por circuito

###########
inputnj_1 <- as.matrix(inputnj_1)
inputct_1 <- as.matrix(inputct_1)
plotef <- dea.plot.frontier(inputnj_1,inputct_1,RTS = "vrs")
###########

tablaf1 <- data.frame(juzgcirc,eficiencia=eftjcirc$thetaOpt)
myColors <- c("firebrick4", "slateblue4")
tablalabelf1 <- data.frame(juzgcirc,eficiencia=eftjcirc$thetaOpt)
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


# Gráfico para juzgados por distrito

###########
input_2 <- as.matrix(input_2)
output_2 <- as.matrix(output_2)
plotef2 <- dea.plot.frontier(input_2,output_2,RTS = "vrs")
###########

tablaf2 <- data.frame(juzgdist,eficiencia=eftjdist$thetaOpt)
tablalabelf2 <- data.frame(juzgdist,eficiencia=eftjdist$thetaOpt)
tablalabelf2$eficiencia[-which(tablalabelf2$eficiencia==1)]=0  
tablalabelf2$color=rep("firebrick4",length(tablalabelf2$DISTRITO))
tablalabelf2$color[which(tablalabelf2$eficiencia==1)]=rep("slateblue4",2)
tablalabelf2$eficiencia = as.factor(tablalabelf2$eficiencia)

tablaff2 <- tablaf2[which(tablaf2$eficiencia==1),] 


fronteraJD <- ggplot(data=tablalabelf2,
                    aes(x=Jueces,
                        y=Egresos,
                        label=DISTRITO)
                    )+
              geom_line(data=tablaff2,
                        aes(x=Jueces,
                            y=Egresos),
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
              ggtitle("Eficiencia técnica proceso reparación directa por distrito (juzgados)"
                      )+
              labs(x="Número de jueces",
                   y="Número de casos resueltos"
                   )+
              geom_label_repel(aes(label=DISTRITO,
                                   color=eficiencia),
                               force=8,
                               arrow = arrow(length = unit(0.5, 'picas'))
                              )
fronteraJD



# Gráfico pra tribunales por distrito

###########
input_3 <- as.matrix(input_3)
output_3 <- as.matrix(output_3)
plotef3 <- dea.plot.frontier(input_3,output_3,RTS = "vrs")
###########

tablaf3 <- data.frame(tribdist,eficiencia=efttdist$thetaOpt)
tablalabelf3 <- data.frame(tribdist,eficiencia=efttdist$thetaOpt)
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







