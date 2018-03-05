# Funci�n DEA



library(readxl)
library(lpSolve)


################################################################
## ORIENTADO A LAS ENTRADAS CON RETTORNOS CONSTANTES A ESCALA ##
################################################################

data <- read_excel("C:/Users/CamiloAndr�s/Desktop/DNP/Proyectos/Distribuci�n de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-C�lculo_DEA.xlsx",sheet = 1)

inputs <- data.frame(data[c(5,6)])
outputs <- data.frame(data[3])
N <- dim(data)[1] # number of DMUs
s <- dim(inputs)[2] # number of inputs
m <- dim(outputs)[2] # number of outputs

f.rhs <- c(rep(0,1,N),1) # RHS constraints
f.dir <- c(rep("<=",1,N),"=") # Direction of constraints
aux <- cbind(-1*inputs,outputs) # matrix of constraints coefficients in (6)


for (i in 1:N) {
  f.obj <- c(0*rep(1,s),as.numeric(outputs[i,])) #objective function coefficients
  f.con <- rbind(aux,c(as.numeric(inputs[i,]),rep(0,1,m))) # add LHS of b^T_Z=1
  results <- lp("max",as.numeric(f.obj),f.con,f.dir,f.rhs,scale=0,compute.sens = TRUE) # solve LPP
  #multipliers <- results$solution #input and output weigths
  #efficiency <- results$objval #efficiency score
  #duals <- results$duals # shadow prices
  if (i==1) {
    weights <- results$solution
    effcrs <- results$objval
    lambdas <- results$duals[seq(1,N)]
  } else {
      weights <- rbind(weights,results$solution)
      effcrs <- rbind(effcrs,results$objval)
      lambdas <- rbind(lambdas,results$duals[seq(1,N)])
    }
  
}

library(xlsx)
library(readxl)
library(lpSolve)


###############################################################
## ORIENTADO A LAS SALIDAS CON RETTORNOS CONSTANTES A ESCALA ##
###############################################################

data <- read_excel("C:/Users/CamiloAndr�s/Desktop/DNP/Proyectos/Distribuci�n de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-C�lculo_DEA.xlsx",sheet = 1)

inputs <- data.frame(data[c(5,6)])
outputs <- data.frame(data[3])
N <- dim(data)[1] # number of DMUs
s <- dim(inputs)[2] # number of inputs
m <- dim(outputs)[2] # number of outputs


f.rhs <- c(rep(0,1,N),1) # RHS constraints
f.dir <- c(rep(">=",1,N),"=") # Direction of constraints
aux <- cbind(inputs, -1*outputs) # matrix of constraints coefficients in

for (i in 1:N) {
  f.obj <- c(as.numeric(inputs[i,]),0*rep(0,1,m)) #objective function coefficients
  f.con <- rbind(aux,c(rep(0,1,s),as.numeric(outputs[i,]))) # add LHS of c^T_z
  results <- lp("min",as.numeric(f.obj),f.con,f.dir,f.rhs,scale=0,compute.sens = TRUE) # solve LPP
  if (i==1) {
    weights <- results$solution
    effcrs <- results$objval
    lambdas <- results$duals[seq(1,N)]
  } else {
    weights <- rbind(weights,results$solution)
    effcrs <- rbind(effcrs,results$objval)
    lambdas <- rbind(lambdas,results$duals[seq(1,N)])
  }
  
}
  


##############################################################
## ORIENTADO A LAS SALIDAS CON RETTORNOS VARIABLES A ESCALA ##
##############################################################

# Para circuitos proceso reparaci�n directa entre CIRCUITOS

datacircj <- read_excel("C:/Users/CamiloAndr�s/Desktop/DNP/Proyectos/Distribuci�n de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-DEA.xlsx",sheet = 1)

inputscircj <- data.frame(datacircj[c(5,6)])
outputscircj <- data.frame(datacircj[3])
N1 <- dim(datacircj)[1] # n�mero unidades tomadoras de decisiones DMUs
s1 <- dim(inputscircj)[2] # n�mero de entradas
m1 <- dim(outputscircj)[2] # n�mero de salidas

f.ldr1 <- c(rep(0,1,N1),1) # lado derecho de la restricci�n
f.dir1 <- c(rep(">=",1,N1),"=") # direcci�n de la restricci�n
mat1 <- cbind(inputscircj, -1*outputscircj,1,-1) # matriz primera restricci�n

for (i in 1:N1) {  # bucle para cada DMU
  f.obj1 <- c(as.numeric(inputscircj[i,]),0*rep(0,1,m1),1,-1) # funci�n objetivo
  f.res1 <- rbind(mat1,c(rep(0,1,s1),as.numeric(outputscircj[i,]),0,0)) # lado izquierdo segunda restricci�n
  resultados1 <- lp("min",as.numeric(f.obj1),f.res1,f.dir1,f.ldr1,scale=0,compute.sens = TRUE) # funci�n de lpSolver
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
# Eficiencia t�cnica
eficiencia1 <- data.frame("Circuito"=datacircj[1],"Eff-t�cnica"=1/effvrs1)
eficiencia1
# Lambdas
lambdas1
# Pesos
pesos1