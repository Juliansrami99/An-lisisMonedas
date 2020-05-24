library(fitdistrplus)
library(shiny)
library(shinydashboard)
library(timeSeries)
library(PerformanceAnalytics)
library(FRAPO)
library(fPortfolio)
library(quantmod)
library(tidyverse)
library(dplyr)
library(fredr)
library(xlsx)
library(plotly)
library(readxl)
library(writexl)
require(reshape2)
library(e1071)
library(poweRlaw)
library(data.table)
library(tseries)
library(mltools)
library(gganimate)
library(cluster)
library(factoextra)
library(NbClust)
library(crosstalk)
library(DT)
library(plyr)
library(dplyr)
#library(openxlsx)


########################################
# Funciones para la descarga y organizacion de datos 
##########################################

convertir<-function(tabla){
  dias=as.data.frame(index(tabla))
  nueva=as.data.frame(tabla)
  nueva=nueva[,-c(1,2,3,6)]
  nueva=as.data.frame(nueva)
  y=cbind(dias,nueva)
  y=as.data.frame(y)
  colnames(y)<-c("DATES","PRECIO","VOLUMEN")
  y$DATES=as.Date(y$DATES)
  u=y %>% distinct(DATES, .keep_all = TRUE)
  #y$PRECIO=as.numeric.factor(y$PRECIO)
  rownames(u)=u$DATES
  return(u)
}

### convertir a numerico de factor
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

### Precio, volumen y dradowns de los datos seleccionados
drawdowns_j<-function(tabla){
  dias=tabla$DATES
  base_serie<-timeSeries(tabla$PRECIO, charvec = rownames(tabla))
  base_retornos<-na.omit(returnseries(base_serie,method = "discrete",
                                      percentage = FALSE, trim = FALSE))
  
  base_draw=PerformanceAnalytics:::Drawdowns(base_retornos)
  tabla=tabla[-1,]
  tabla_nueva=cbind(tabla[,1],base_retornos,tabla[,3],base_draw$TS.1,tabla$PRECIO)
  tabla_nueva=as.data.frame(tabla_nueva)
  colnames(tabla_nueva)=c("DATES","RETORNOS","VOLUMEN","DRAWDOWNS","PRECIO")
  tabla_nueva$DATES=as.Date(tabla_nueva$DATES)
  return(tabla_nueva)
}

drawdowns_simulados<-function(tabla){
  dias=tabla$DATES
  base_serie<-timeSeries(tabla$PRECIO, charvec = rownames(tabla))
  base_retornos<-na.omit(returnseries(base_serie,method = "discrete",
                                      percentage = FALSE, trim = FALSE))
  
  base_draw=PerformanceAnalytics:::Drawdowns(base_retornos)
  tabla=tabla[-1,]
  tabla_nueva=cbind(tabla[,1],tabla$PRECIO,base_draw$TS.1)
  tabla_nueva=as.data.frame(tabla_nueva)
  colnames(tabla_nueva)=c("DATES","PRECIO","DRAWDOWNS")
  return(tabla_nueva)
}

### Graficar precio con ploltly ###
grafica_precio<-function(tabla){
  g<-ggplot(tabla,aes(x=tabla$DATES,y=tabla$PRECIO))+
    geom_line()
  ggplotly(g)
}

##### todos: Funcion mas importante #####

todos<-function(vec){
  total=list()
  for(j in 1:length(vec)){
    tryCatch({a=getSymbols(vec[j],src="yahoo",from="1927-12-30",auto.assign=FALSE,env = NULL)
    },error=function(e)NA)
    total[[j]]<-a
    
  }
  precios=list()
  for (i in 1:length(total)){
    b=convertir(total[[i]])
    precios[[i]]<-b
  }
  drawdowns=list()
  for (u in 1:length(precios)){
    precios[[u]]=na.omit(precios[[u]])
    c=drawdowns_j(precios[[u]])
    drawdowns[[u]]<-c
  }
  defini=list()
  for (u in 1:length(drawdowns)){
    drawdowns[[u]]=na.omit(drawdowns[[u]])
    c=dup2(drawdowns[[u]])
    ultima_actua=medida(c)
    defini[[u]]<-ultima_actua
  }
  return(defini)
}


# Para filtrar y que todos los dataframes inicien desde una misma fecha
lista_definitiva=function(lista){
  dias=c()
  for (i in 1:length(lista)){
    dias=c(dias,lista[[i]][1,1])
  }
  dia_minimo=as.Date(max(dias))
  lista_final=list()
  for (j in 1:length(lista)){
    tabla=lista[[j]] %>% filter(DATES>=dia_minimo)
    lista_final[[j]]=tabla
  }
  return(lista_final)
}

# todo en una misma tabla
tabla_definitiva=function(lista,paises){
  tabla_final=cbind(lista[[1]]$DATES,lista[[1]]$DRAWDOWNS)
  for (i in 2:length(lista)){
    tabla_final=cbind(tabla_final,lista[[i]]$DRAWDOWNS)
  }
  tabla_final=as.data.frame(tabla_final)
  colnames(tabla_final)=c("DATES",paises)
  tabla_final$DATES=as.Date(tabla_final$DATES)
  return(tabla_final)
}

# plotear todos los drawdowns
plot_drawdowns=function(tabla,paises){
  x11()
  plot(tabla$DATES,tabla[,2],ylim = c(-1,0),type = "l",xlab="Tiempo",ylab="Drawdowns")
  cl <- rainbow(ncol(tabla))
  for (i in 3:ncol(tabla)){
    lines(tabla$DATES,tabla[,i],col = cl[i],type = 'l')
  }
  legend("bottomleft",legend = c(paises),col=c("black",cl[3:ncol(tabla)]),bty="n",lty=1)
}

# Graficas descriptivas
graficas<-function(datos2){
  vector_minimos<-c()
  vector_maximos<-c()
  for (j in 1:ncol(datos2)){
    minimo=min(datos2[,j])
    maximo=max(datos2[,j])
    vector_minimos=c(vector_minimos,minimo)
    vector_maximos=c(vector_maximos,maximo)
  }
  minimo=min(vector_minimos)
  maximo=max(vector_maximos)
  numCols = ncol(datos2)
  x11()
  par(mfrow=c(numCols,numCols))
  par(mar=c(1,1,1,1))
  for(name1 in colnames(datos2)){
    for(name2 in colnames(datos2)){
      var1<-datos2[[name1]]
      var2<-datos2[[name2]]
      if(name1==name2){
        hist(var1,main=name1,yaxt='n',breaks = seq(min(var1),max(var1),l=10),
             col="blue")
      }else{
        plot(var2,var1,xlab=name1,ylab=name2,cex=1,pch=20,xaxt='n',yaxt='n',xlim=c(minimo,maximo),ylim=c(minimo,maximo),main=paste(name1," vs ",name2,sep=""))
      }
    } 
  } 
}


dendograma_cut<-function(tablas_original,vec){
  menor=nrow(tablas_original[[1]])
  indice=1
  for (i in 1:length(tablas_original)) {
    if (nrow(tablas_original[[i]])<menor){
      menor=nrow(tablas_original[[i]])
      indice=i
    }
  }
  listas_final=list()
  dias_minimo=tablas_original[[indice]]$DATES[1]
  for (j in 1:length(tablas_original)) {
    tabla_temp=tablas_original[[j]] %>% filter(DATES>=dias_minimo)
    tabla_temp=as.data.frame(tabla_temp)
    listas_final[[j]]<-tabla_temp
  }
  
  tabl=data.frame("seq"=seq(1,nrow(listas_final[[1]])))
  for (a in 1:length(listas_final)){
    tabl=cbind(tabl,listas_final[[a]]$DRAWDOWNS)
  }
  tabl=tabl[,-1]
  colnames(tabl)<-vec
  tabl_tran=t(tabl)
  tabl_tran=as.data.frame(tabl_tran)
  # distancia euclidiana
  d<-dist(as.matrix(tabl_tran),method = "euclidean")
  
  # numero de clusters
  # gap_stat <- clusGap(tabl_tran, FUN = hcut, nstart = 25, K.max = 10, B = 50)
  # a<-fviz_gap_stat(gap_stat)
  clus=8
  fit<-hclust(d)
  sub_grp <- cutree(fit, k = clus)
  return(list(fit,sub_grp,clus,tabl_tran))
}





paises=c("KRWUSD=X","COPUSD=X","MXNUSD=X","CLPUSD=X","TRYUSD=X","ZARUSD=X",
         "BRLUSD=X","PLNUSD=X","PENUSD=X","IDRUSD=X","THBUSD=X","EGPUSD=X",
         "RUBUSD=X","INDUSD=X")

nombre_paises=c("Corea del Sur","Colombia","México","Chile","Turquía","Sudáfrica",
                "Brasil","Polonia","Perú","Indonesia","Tailandia","Egipto","Rusia","India")

monedas=c("KRW","COP","MXN","CLP","TRY","ZAR","BRL","PLN","PEN","INR","THB","EGP","RUB","IND")

# Paso 1: Se crea un lista llamada julian donde se tienen 14 dataframes con diferentes variables economicas
julian=todos_numero(paises,1)

# Paso 2: Se modifica la lista para que todas las tablas inicien desde el mismo día
jul=lista_definitiva(julian)

# Paso 3: Crear una sola tabla con todas las variables
jul_tabla=tabla_definitiva(jul,nombre_paises)

# Paso 4: Se grafican todos los drawdowns
plot_drawdowns(jul_tabla,nombre_paises)

# Paso 5: Gráficas descriptivas
su=jul_tabla[,c(2:ncol(jul_tabla))]
colnames(su)<-monedas
graficas(su)

# Paso 6: Clusters
kk=t(su)
d<-dist(as.matrix(kk),method = "euclidean")
clus=9
#dendograma
fit<-hclust(d)
x11()
plot(fit)
# Cutree
library(cluster)
x11()
sub_grp <- cutree(fit, k = 4)

x11()
eclust(kk, "kmeans", k=4)


y=lm(su$ZAR~su$EGP,data=su)

x11()
plot(y$residuals,ylim=c(0,0.0005))





