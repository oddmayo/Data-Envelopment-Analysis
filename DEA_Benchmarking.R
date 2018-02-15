# DEA rama judicial paquete Benchmarking

library(Benchmarking)
library(readxl)
library(directlabels)
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

e <- dea(XREF=input,YREF=output,input,output, RTS = "variable", model = "output")
e
e$thetaOpt

# data frames para construir el ggplot

tabla <- data.frame(data,eficiencia=e$thetaOpt)

tabla2 <- tabla[which(tabla$eficiencia==1),] 
tabla <- tabla[-which(tabla$eficiencia==1),]

# ggplot 

efplot <- ggplot(data=tabla,aes(x=jueces_civil,y=sali_civil,label=dmu))+
  geom_point()+
  geom_label_repel(aes(label=dmu),size=2.5,color="navy")+
  geom_line(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="darkred",cex=1.25,linetype="longdash")+
  geom_point(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="blue",cex=2.5)+
  geom_label_repel(data = tabla2,aes(label=dmu),size=2.5,color="red",fill="green")+
  labs(x="Número de jueces",y="Número de casos resueltos")+
  ggtitle("Eficiencia técnica especialidad civil")+
  theme(rect=element_rect(fill = "transparent"),plot.title = element_text(hjust = 0.5))

efplot


ggsave("frontera_civil.png",efplot,dpi = 720, bg="transparent")


ggplotly(a)




