# DEA rama judicial paquete Benchmarking

library(Benchmarking)
library(readxl)
library(directlabels)
library(plotrix)

# Cargar datos en su totalidad
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/Base filtrada/Base esta si es.xlsx")
# Cargar datos solo especialidad civil
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/BaseCivil.xlsx")


# Dataframes para función dea
inputdata <- data.frame(data[3])
outputdata <- data.frame(data[2])


input <- as.matrix(inputdata)
output <- as.matrix(outputdata)

# parámetros para personalizar gráfico
par(bg="white",col.axis="black",font=2,cex.axis=1)

plot(input, output,col="blue",type = 'p',main = "Eficiencia",pch=19,panel.first = grid(col = "grey",lty = 1,lwd=1))
# segments(0,y[2],x[2],y[2],add=T,lty=2,col="green",lwd=3)


# plot frontera de producción
dea.plot.frontier(input,output,
                  xlab="# jueces",
                  ylab="# casos resueltos",
                  pch=20,
                  lwd=1,
                  lty=5,
                  col="blue",
                  cex.axis=1,
                  xlim=range(0:200),
                  main="Eficiencia técnica especialidad civil"
                  )

# texto para los puntos del plot
spread.labels(input,output,
              labels = data$dmu
              )

