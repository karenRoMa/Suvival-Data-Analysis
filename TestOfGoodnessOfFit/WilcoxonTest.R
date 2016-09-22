#Gehan's Generalized Wilcoxon Test

#Estructura de los datos a introducir en la funcion 
CMF <- data.frame(time = c(23,16,18,20,24),status = c(0,1,1,1,1))
Control <- data.frame(time = c(15,18,19,19,20), status = c(0,0,0,0,0))

#Donde la variable status indica datos censurados con
# 0 : no censurado
# 1 : censurado

wilcoxon <- function(data1,data2,alpha){
  #Usando procedimiento de Cox-Mantel
  n1 = length(data1$time)
  n2 = length(data2$time)
  data1$group = 1
  data2$group = 2
  data <- data.frame(time= c(data1$time,data2$time),status= c(data1$status,data2$status),group=c(data1$group,data2$group))
  sortData <- data[order(data$time,data$status),] 
  n = length(sortData$time)
  sortData$rank1 = 0
  sortData$rank2 = 0
  count = 1
  
  for(i in 1:n){
    if(sortData$status[i] == 0){
      sortData$rank1[i] = count
      count = count +1
    }
  }
  
  for(j in 2: n){
    if(sortData$status[j] == 1){ 
      if (sortData$rank1[j-1] != 0){
        sortData$rank2[j] = sortData$rank1[j-1] + 1
      }
    }
  }
  
  value = 0
  for(k in 1:n){
    l=1
    while (sortData$time[k] == sortData$time[k+l] && sortData$status[k] == 0 && sortData$status[k+l] == 0) {
      value = sortData$rank3[k]
      l=l+1
    }
    if(sortData$time[k] == sortData$time[k+1] && sortData$status[k] == 0 && sortData$status[k+1] == 0){
      sortData$rank3[k] = value  
    }else{
      value = 0
    }
  }
  
  sortData$R1 = sortData$rank1 + sortData$rank2
  sortData$rank3 = n:1
  sortData$rank4 = 0
  
  value = 0
  for(h in 1:n){
    l=1
    while (sortData$time[h] == sortData$time[h+l] && sortData$status[h] == 0 && sortData$status[h+l] == 0) {
      value = sortData$rank3[h+l]
      l=l+1
    }
    if(sortData$time[h] == sortData$time[h+1] && sortData$status[h] == 0 && sortData$status[h+1] == 0){
      sortData$rank3[h] = value  
    }else{
      value = 0
    }
  }
  
  sortData$R2 = sortData$rank3
  for(y in 1:n){
    if(sortData$status[y] == 1){
      sortData$R2[y] = 1
    }
  }
  
  sortData$Ui = sortData$R1 - sortData$R2
  
  W = 0
  for (u in 1:n){
    if(sortData$group[u] == 1){
      W = W + sortData$Ui[u]
    }
  }
  
  sumaUis = sum(sortData$Ui^2)
  varW = (n1*n2*sumaUis/((n1+n2)*(n1+n2-1)))
  Z = W / sqrt(varW)
  
  zNorm = qnorm(1-alpha)
  
  if (Z > zNorm){
    cat("Se rechaza H0 a un nivel de ",alpha, ". Ya que Z =",Z,">",zNorm)
  }else{
    cat("No se rechaza H0 a un nivel de ",alpha, ". Ya que Z =",Z,"<",zNorm)
  }
  #Descomentar esta linea si se quiere obtener el valor de Z
  #return(Z)
}

#Ejemplo de uso
wilcoxon(CMF,Control,.001)
