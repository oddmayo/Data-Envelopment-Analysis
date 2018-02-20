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

# Bogotá indica no ser un dato atípico ante la poca variación entre resultados 






