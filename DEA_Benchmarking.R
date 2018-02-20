# DEA rama judicial paquete Benchmarking

library(Benchmarking)
library(readxl)
library(plotrix)
library(plotly)
library(ggplot2)
library(ggrepel)


# Cargar datos en su totalidad
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/Base filtrada/Base esta si es.xlsx")
# Cargar datos solo especialidad civil
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/BaseCivil.xlsx")




# Dataframes para función dea
inputdata <- data.frame(data[3])
outputdata <- data.frame(data[2])


input <- as.matrix(inputdata)
output <- as.matrix(outputdata)



#ef

ef <- dea(input,output,RTS = "vrs",ORIENTATION = "out")

# parámetros para personalizar gráfico
par(bg="white",col.axis="black",font=2,cex.axis=1)

plot(input, output,col="blue",type = 'p',main = "Eficiencia",pch=19,panel.first = grid(col = "grey",lty = 1,lwd=1))
# segments(0,y[2],x[2],y[2],add=T,lty=2,col="green",lwd=3)


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

# Eficiencia de paquete rDEA 
library(rDEA)
e <- dea(XREF=input,YREF=output,input,output, RTS = "variable", model = "output")
e
e$thetaOpt

# data frames para construir el ggplot

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
                  theme(legend.position = "none",rect=element_rect(fill = "transparent"),plot.title = element_text(hjust = 0.5))+
                  ggtitle("Eficiencia técnica especialidad civil")+
                  labs(x="Número de jueces",y="Número de casos resueltos")+
                  geom_label_repel(aes(label=dmu,color=eficiencia),force=8,arrow = arrow(length = unit(0.5, 'picas'))
                  )

efplot2

ggsave("frontera_civil3.png",efplot2,dpi = 700, bg= "transparent")
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

# plotly: me crashea R 
ggplotly(efplot2)



# NUEVAS EFICIENCIAS

# Eficiencia de Escala
library(rDEA)
e_vrs <- dea(XREF = input,YREF = output,input,output,model = "output",RTS = "variable")
e_crs <- dea(XREF = input,YREF = output,input,output,model = "output",RTS = "constant")
efescala <- e_crs$thetaOpt/e_vrs$thetaOpt
efescala

tablaescala <- data.frame(Departamento=data$dmu,EficienciaEscala=efescala)
tablaescala


# Eficiencia de asignación 
# Para esta es necesario tener costos/precios
# Para el caso de los jueces podría servir su salario


# Super eficiencia

superef <- sdea(input,output,RTS = "vrs",ORIENTATION = "out")
superef$eff
tablasuperef <- data.frame(Departamento=data$dmu, superef$eff)
tablasuperef
# Arauca no tiene solución factible para super eficiencia
# esto puesto que presenta rendimientos crecientes a escala
# Arauca puede reducir su output de manera proporcional preservando su eficiencia
# La eficiencia de Arauca es estable ante cambios proporcionales de los datos

# Variables no discrecionarias
# Variables fijas, sobre las que las unidades a evaluar no tienen control
# pero que de alguna manera afectan su eficiencia



# Holguras
# La maximización de la segunda etapa puede verse como
# detectar la mejor unidad de referencia

# El DEA en 2 etapas tiene la desventaja de que los resultados varían con la unidades de medición
 # peeero eso se corrije con la función que contiene el no arquimediano
library(Benchmarking)
dosestapas <- dea(input,output,RTS="vrs",ORIENTATION = "out",SLACK = TRUE)
dosestapas$slack
dosestapas$sx
dosestapas$sy


# Desde el punto de vista de la teoría de la dualidad
# DEA hace la mejor comparación posible entre DMUs

# OUTLIERS

boxplot(data$sali_civil)
boxplot(data$jueces_civil)


scatterplot3d(data$sali_civil, data$jueces_civil)

atipico <- outlier.ap(input,output,NDEL=5)

outlier.ap.plot(atipico$ratio)


# Detección de datos atípicos con nube de datos

x <- with(data, cbind(data$jueces_civil))
y <- with(data, cbind(data$sali_civil))
xy <- cbind(x,y)
D <- det(t(xy)%*%xy)
i <- c(1) # firma o firmas a quitarse
xyi = xy[-i,]
Di <- det( t(xyi) %*% xyi )
Ri <- Di/D
Ri

# La firma 5 (Bogotá), evidencia ser un dato atípico

# ANÁLISIS ESTADÍSTICO




