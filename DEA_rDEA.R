# DEA rama judicial paquete rDEA

# Cargar librerías
library(rDEA)
library(readxl)



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


# Eficiencias feo
e <- dea(XREF=input,YREF=output,input,output, RTS = "variable", model = "output")
e
summary(e)
e$thetaOpt













