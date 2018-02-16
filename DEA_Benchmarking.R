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

# ggplot 2

efplot2 <- ggplot(data=tablalabel,aes(x=jueces_civil,y=sali_civil,label=dmu))+
  geom_line(data=tabla2,aes(x=jueces_civil,y=sali_civil),color="firebrick4",cex=1,linetype="F1")+
  geom_point(aes(color=eficiencia,size=eficiencia))+
  scale_color_manual(values=myColors)+
  theme(legend.position = "none",rect=element_rect(fill = "transparent"),plot.title = element_text(hjust = 0.5))+
  ggtitle("Eficiencia técnica especialidad civil")+
  labs(x="Número de jueces",y="Número de casos resueltos")+
  geom_label_repel(aes(label=dmu,color=eficiencia),force=8,arrow = arrow(length = unit(0.5, 'picas')))

efplot2

ggsave("frontera_civil3.png",efplot2,dpi = 700, bg="transparent")
# ggplot 

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
geom_label_repel

ggsave("frontera_civil2.png",efplot,dpi = 700, bg="transparent")

getwd()
ggplotly(efplot)




