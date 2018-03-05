# Intento de DEA



library(readxl)
library(lpSolve)
data <- read_excel("C:/Users/CamiloAndrés/Desktop/Ejemploparaporgramar.xlsx")

inputs <- data.frame(data[2])
outputs <- data.frame(data[c(3,4,5)])
N <- dim(data)[1] # number of DMUs
s <- dim(inputs)[2] # number of inputs
m <- dim(outputs)[2] # numbre of outputs

# input oriented, crs and multiplier form


#############
# INTENTO # 1
#############
data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 1)

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


# Rendimientos constantes y orientado a output

data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 1)

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
  


# Rendimientos variables y orietado a output!!!!!!


data <- read_excel("C:/Users/CamiloAndrés/Desktop/DNP/Proyectos/Distribución de la oferta judicial/DEA en R/Repo/Data-Envelopment-Analysis/Bases_de_Rama_Judicial-Cálculo_DEA.xlsx",sheet = 1)

inputs <- data.frame(data[c(5,6)])
outputs <- data.frame(data[3])
N <- dim(data)[1] # number of DMUs
s <- dim(inputs)[2] # number of inputs
m <- dim(outputs)[2] # number of outputs


f.rhs <- c(rep(0,1,N),1) # RHS constraints
f.dir <- c(rep(">=",1,N),"=") # Direction of constraints
aux <- cbind(inputs, -1*outputs,1,-1) # matrix of constraints coefficients in

for (i in 1:N) {
  f.obj <- c(as.numeric(inputs[i,]),0*rep(0,1,m),1,-1) #objective function coefficients
  f.con <- rbind(aux,c(rep(0,1,s),as.numeric(outputs[i,]),0,0)) # add LHS of c^T_z
  results <- lp("min",as.numeric(f.obj),f.con,f.dir,f.rhs,scale=0,compute.sens = TRUE) # solve LPP
  multipliers <- results$solution
  u0 <- multipliers[s+m+1]-multipliers[s+m+2]
  if (i==1) {
    weights <- c(multipliers[seq(1,s+m)],u0)
    effvrs <- results$objval
    lambdas <- results$duals[seq(1,N)]
  } else {
    weights <- rbind(weights,c(multipliers[seq(1,s+m)],u0))
    effvrs <- rbind(effvrs,results$objval)
    lambdas <- rbind(lambdas,results$duals[seq(1,N)])
  }
  
}
