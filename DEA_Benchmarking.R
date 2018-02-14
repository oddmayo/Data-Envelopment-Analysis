# DEA rama judicial paquete Benchmarking

library(Benchmarking)
library(readxl)



# Cargar datos en su totalidad
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/Base filtrada/Base esta si es.xlsx")
# Cargar datos solo especialidad civil
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/BaseCivil.xlsx")


# Dataframes para función dea
inputdata <- data.frame(data[3])
outputdata <- data.frame(data[2])


input <- as.matrix(inputdata)
output <- as.matrix(outputdata)


# Eficiencia
e <- dea(XREF=inputdata,YREF=outputdata,inputdata,outputdata, RTS = "vrs", ORIENTATION = "out")
eff(e)
print(e)
summary(e)
lambda(e)


dea.plot(input, output, RTS="vrs",ORIENTATION = "out",txt = rownames(input))

text(1, 1, "Works just like it does now", font=2)
text(1, 1, "Same effect as above", font=list(face="italic"))
text(1, 1, "Same effect as above", font=list(face=2))
text(1, 1, "Change the font just for this text",
     font=list(family="Helvetica", face="bold-italic"))
text(1:4, 1:4, paste("Font face", 1:4, "for this text"),
     font=list(family="Courier New", face=1:4))

# parámetros para personalizar gráfico
par(bg="white",col.axis="black",font=2,cex.axis=1)

plot(input, output,col="blue",type = 'p',main = "Eficiencia",pch=19,panel.first = grid(col = "grey",lty = 1,lwd=1))
dea.plot(input,output,main="Frontera eficiencia",GRID=T,lty=3,lwd=2,col="red",RTS = "vrs", ORIENTATION = "out")
segments(0,y[2],x[2],y[2],add=T,lty=2,col="green",lwd=3)


# plot frontera de producción

dea.plot.frontier(input,output)



e <- dea(x,y, RTS="vrs",ORIENTATION = "out")
