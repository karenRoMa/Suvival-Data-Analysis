#---------------------Ejemplo de Tabla Clinica--------------------
#Ejemplo de Tabla Clinica
#Rodriguez Macias Karen Viviana

#Utilizaremos esta funcion para hacer los puntos medios
puntoMedio <- function(x){
  p<- rep(0,length(x)) 
  for(i in 1:length(x)-1){
    p[i]= (x[i] + x[i+1])/2
  } 
  p[length(x)] = NA 
  return(p)
}


#Datos iniciales y estructura de la tabla
#Intervalo del tiempo observado
tiempos <- seq(0,36,by=3) 
n<- length(tiempos)
#Punto Medio del intervalo
puntosMedios <- puntoMedio(tiempos)
#Longitud del intervalo
long <- c(rep(3,length(tiempos)-1),NA)
# Numero de individuos a los que se les perdio seguimiento en el intervalo
set.seed(8)
seguimientoPerdido <- c(0,rpois(n-2,14),0)
# Numero de individuos censurados por la derecha
set.seed(8)
censurados <- c(0,rpois(n-2,30),0)
#Numero de individuos con falla
set.seed(8)
fallas <- c(rpois(n-1,60),0)
#Numero de individuos que entran en el intervalo
entrando <- c(887,rep(0,n-1))
for(k in 2:n){
  entrando[k]= entrando[k-1] - seguimientoPerdido[k-1] - censurados[k-1]
}
# Numero de expuestos
expuestos <- rep(0,n)
#Proporcion condicional de falla
pcFallas <- rep(0,n)
#Proporcion condicional de supervivencia
pcSup <- rep(0,n)
#Variable de supervivencia estimada
supEst <- rep(0,13)
#Variable de funcion de densidad estimada
fEst <- rep(0,13)
#Variable de funcion de riesgo estimada
hEst <- rep(0,13)

for(j in 1:n) {
  expuestos[j]= entrando[j] - 0.5*(seguimientoPerdido[j]+ censurados[j])
  if(fallas[j]==0){
    pcFallas[j]= 1
  }else{
    pcFallas[j]= fallas[j]/expuestos[j]
  }
  pcSup[j]=1-pcFallas[j]
  hEst[j] = fallas[j]/(long[j] * (expuestos[j]-(fallas[j]/2)))
  
  if(j==1){
    supEst[j] = 1
  }else{
    supEst[j]=round(supEst[j-1]*pcSup[j-1],digits = 5)
  }
  if(j!=n){
    fEst[j] = (supEst[j]*pcFallas[j])/long[j] 
  }else{
    fEst[j] = NA
  }
}


#Juntamos todo en una tabla 
TablaClinica <- data.frame(tiempos,puntosMedios,long,seguimientoPerdido,censurados,fallas,entrando,expuestos,supEst,fEst,hEst)
View(TablaClinica)

#Desviacion estandar de la supervivencia estimada
sdSupEst <- sd(supEst,na.rm = TRUE)

#Desviacion estandar de la funcion de densidad estimada
sdFEst <- sd(fEst,na.rm = TRUE)

#Intervalos de confianza
interFSup <- fEst[1:n-1]
interFSup <- interFSup + sdFEst
interFInf <- fEst[1:n-1]
interFInf <- interFInf - sdFEst

#Desviacion estandar de la funcion de riesgo estimada
sdHEst <- sd(hEst,na.rm = TRUE)
#Intervalos de confianza
interHSup <- hEst[1:n-1]
interHSup <- interHSup+sdFEst
interHInf <- hEst[1:n-1]
interHInf <- interHInf-sdFEst

plot(tiempos,supEst,xlab = "Tiempo",ylab = "Supervivencia Estimada",main = "Función de Supervivencia Estimada")
lines(tiempos,supEst-sdSupEst,col="purple")
lines(tiempos,supEst+sdSupEst,col="purple")

plot(tiempos,fEst, xlab = "Tiempo",ylab = "Función de Densidad",main = "Función de Densidad Estimada")
lines(tiempos[1:n-1],interFInf,col="blue")
lines(tiempos[1:n-1],interFSup,col="blue")

plot(tiempos,hEst,xlab = "Tiempo",ylab = "Función de Riesgo", main = "Función de Riesgo Estimada")
lines(tiempos[1:n-1],interHInf,col="orange")
lines(tiempos[1:n-1],interHSup,col="orange")
