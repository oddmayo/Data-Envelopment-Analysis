####################################
## ANÁLISIS DE ENVOLTURA DE DATOS ##
####################################

# PAQUETES
library(readxl)
library(plotrix)
library(plotly)
library(ggplot2)
library(ggrepel)
library(zoo)

#############################
######### EJEMPLOS ##########
############################

# Cargar datos en su totalidad
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/Base filtrada/Base esta si es.xlsx")
# Cargar datos solo espcialidad civil
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/BaseCivil.xlsx")

# dataframe para la función dea
inputdata <- data.frame(data[3])
outputdata <- data.frame(data[2])
input <- as.matrix(inputdata)
output <- as.matrix(outputdata)

# Test de rendimientos a escala
rendimientos <- rts.test(input,output,model="output",H0="constant",bw="cv", B=100,alpha = 0.05)
rendimientos$pvalue

# Eficiencia técnica
e <- dea(XREF=input,YREF=output,input,output, RTS = "variable", model = "output")
e
summary(e)
e$thetaOpt
tabla_eficiencias <- data.frame(Departamento=data$dmu,"Eficiencia_técnica"=e$thetaOpt)
tabla_eficiencias

# Eficiencia sin Bogotá
datos2 <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/BaseCivilSinBogota.xlsx")

inputdata2 <- data.frame(datos2[3])
outputdata2 <- data.frame(datos2[2])
input2 <- as.matrix(inputdata2)
output2 <- as.matrix(outputdata2)

ef <- dea(XREF=input2,YREF=output2,input2,output2, RTS = "variable", model = "output")
ef

tabla2 <- data.frame(Departamento=datos2$dmu,Eficiencia_técnica=e$thetaOpt)
tabla2

# plot frontera de producción
frontera <- dea.plot.frontier(input,output,
                              xlab="# jueces",
                              ylab="# casos resueltos",
                              pch=20,
                              lwd=1,
                              lty=5,
                              col="red",
                              cex.axis=1,
                              cex = 1,
                              xlim=range(0,150),
                              ylim=range(0,20000),
                              main="Eficiencia técnica especialidad civil"
)

# dataframes para el gráfico

tabla <- data.frame(data,eficiencia=e$thetaOpt)

tablalabel <- data.frame(data,eficiencia=e$thetaOpt)
tablalabel$eficiencia[-which(tablalabel$eficiencia==1)]=0  

tablalabel$color=rep("firebrick4",length(tablalabel$dmu))
tablalabel$color[which(tablalabel$eficiencia==1)]=rep("slateblue4",3)


tablalabel$eficiencia = as.factor(tablalabel$eficiencia)

myColors <- c("firebrick4", "slateblue4")



tabla2 <- tabla[which(tabla$eficiencia==1),] 
tabla <- tabla[-which(tabla$eficiencia==1),]

# ggplot con un mejor trato para los labels
# cortesía de Laura
efplot2 <- ggplot(data=tablalabel,aes(x=jueces_civil,y=sali_civil,label=dmu))+
  geom_line(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="firebrick4",cex=1,linetype="F1")+
  geom_point(aes(color=eficiencia,size=eficiencia))+
  scale_color_manual(values=myColors)+
  theme(legend.position = "none",rect=element_rect(fill = "transparent"),plot.title = element_text(hjust = 0.5),
        # para fondo blanco
        panel.grid=element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
  )+
  ggtitle("Eficiencia técnica especialidad civil")+
  labs(x="Número de jueces",y="Número de casos resueltos")+
  geom_label_repel(aes(label=dmu,color=eficiencia),force=8,arrow = arrow(length = unit(0.5, 'picas'))
  )

efplot2

ggsave("frontera_civil3.png",efplot2,dpi = 700, bg= "white")

# ggplot teniendo que usar "force"

efplot <- ggplot(data=tabla,aes(x=jueces_civil,y=sali_civil,label=dmu))+
  geom_point(color="firebrick4")+
  geom_label_repel(aes(label=dmu),size=2,color="dimgrey",force=8,arrow = arrow(length = unit(0.01, 'npc')))+
  geom_line(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="firebrick4",cex=1,linetype="F1")+
  geom_point(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="slateblue4",cex=2.5)+
  geom_label_repel(data = tabla2,aes(label=dmu),size=3,color="white",fill="slateblue4",force=800)+
  labs(x="Número de jueces",y="Número de casos resueltos")+
  ggtitle("Eficiencia técnica especialidad civil")+
  theme(rect=element_rect(fill = "transparent"),plot.title = element_text(hjust = 0.5))

efplot

# Guardar .png 
getwd()
ggsave("frontera_civil2.png",efplot,dpi = 700, bg="transparent")




# EJEMPLOS rDEA


## load data on Japanese hospitals (Besstremyannaya 2013, 2011)
data("hospitals", package="rDEA")

## inputs and outputs for analysis
Y = hospitals[c('inpatients', 'outpatients')]
X = hospitals[c('labor', 'capital')]
W = hospitals[c('labor_price', 'capital_price')]

## Naive input-oriented DEA score for the first 20 firms under variable returns-to-scale
firms=1:20
di_naive = dea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], model="input", RTS="variable")
di_naive$thetaOpt

## Naive DEA score in cost-minimization model for the first 20 firms under variable returns-to-scale
ci_naive = dea(XREF=X, YREF=Y, X=X[firms,], Y=Y[firms,], W=W[firms,],
               model="costmin", RTS="variable")
ci_naive$XOpt
ci_naive$gammaOpt


