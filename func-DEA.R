library(xlsx)
library(readxl)
library(lpSolve)

##############################################################
## ORIENTADO A LAS SALIDAS CON RETTORNOS VARIABLES A ESCALA ##
##############################################################

# Para juzgados proceso reparación directa entre CIRCUITOS

datacircj <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 1)

inputscircj <- data.frame(datacircj[c(5,6)])
outputscircj <- data.frame(datacircj[3])
N1 <- dim(datacircj)[1] # número unidades tomadoras de decisiones DMUs
s1 <- dim(inputscircj)[2] # número de entradas
m1 <- dim(outputscircj)[2] # número de salidas

f.ldr1 <- c(rep(0,1,N1),1) # lado derecho de la restricción
f.dir1 <- c(rep(">=",1,N1),"=") # dirección de la restricción
mat1 <- cbind(inputscircj, -1*outputscircj,1,-1) # matriz primera restricción

for (i in 1:N1) {  # bucle para cada DMU
  f.obj1 <- c(as.numeric(inputscircj[i,]),0*rep(0,1,m1),1,-1) # función objetivo
  f.res1 <- rbind(mat1,c(rep(0,1,s1),as.numeric(outputscircj[i,]),0,0)) # lado izquierdo segunda restricción
  resultados1 <- lp("min",as.numeric(f.obj1),f.res1,f.dir1,f.ldr1,scale=0,compute.sens = TRUE) # función de lpSolver
  multiplicadores1 <- resultados1$solution
  u01 <- multiplicadores1[s1+m1+1]-multiplicadores1[s1+m1+2]
  if (i==1) { # para los DMUs eficientes
    pesos1 <- c(multiplicadores1[seq(1,s1+m1)],u01)
    effvrs1 <- resultados1$objval
    lambdas1 <- resultados1$duals[seq(1,N1)]
  } else {    # para los DMUs ineficientes
    pesos1 <- rbind(pesos1,c(multiplicadores1[seq(1,s1+m1)],u01))
    effvrs1 <- rbind(effvrs1,resultados1$objval)
    lambdas1 <- rbind(lambdas1,resultados1$duals[seq(1,N1)])
  }
}
# Eficiencia técnica
eficiencia1 <- data.frame("Circuito"=datacircj[1],"Eff-técnica"=1/effvrs1)
eficiencia1
# Lambdas
lambdas1
# Pesos
pesos1


# Para juzgados proceso reparación directa entre DISTRITOS (No interpretable/Solo comparación)

datadistj <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 4)

inputsdistj <- data.frame(datadistj[c(5,6)])
outputsdistj <- data.frame(datadistj[3])
N2 <- dim(datadistj)[1] # número unidades tomadoras de decisiones DMUs
s2 <- dim(inputsdistj)[2] # número de entradas
m2 <- dim(outputsdistj)[2] # número de salidas

f.ldr2 <- c(rep(0,1,N2),1) # lado derecho de la restricción
f.dir2 <- c(rep(">=",1,N2),"=") # dirección de la restricción
mat2 <- cbind(inputsdistj, -1*outputsdistj,1,-1) # matriz primera restricción

for (i in 1:N2) {  # bucle para cada DMU
  f.obj2 <- c(as.numeric(inputsdistj[i,]),0*rep(0,1,m2),1,-1) # función objetivo
  f.res2 <- rbind(mat2,c(rep(0,1,s2),as.numeric(outputsdistj[i,]),0,0)) # lado izquierdo segunda restricción
  resultados2 <- lp("min",as.numeric(f.obj2),f.res2,f.dir2,f.ldr2,scale=0,compute.sens = TRUE) # función de lpSolver
  multiplicadores2 <- resultados2$solution
  u02 <- multiplicadores2[s2+m2+1]-multiplicadores2[s2+m2+2]
  if (i==1) { # para los DMUs eficientes
    pesos2 <- c(multiplicadores2[seq(1,s2+m2)],u02)
    effvrs2 <- resultados2$objval
    lambdas2 <- resultados2$duals[seq(1,N2)]
  } else {    # para los DMUs ineficientes
    pesos2 <- rbind(pesos2,c(multiplicadores2[seq(1,s2+m2)],u02))
    effvrs2 <- rbind(effvrs2,resultados2$objval)
    lambdas2 <- rbind(lambdas2,resultados2$duals[seq(1,N2)])
  }
}
# Eficiencia técnica
eficiencia2 <- data.frame(datadistj[1],"Eff-técnica"=1/effvrs2)
eficiencia2
# Lambdas
lambdas2
# Pesos
pesos2
