rm(list = ls())
require(readxl)


                                        ### PROYECTO VIH ###

## LEER DATOS ##

# Lectura del archivo, funcion que resibe la ubicacion del archivo. Ejemplo : "C:/Users/Breayann/Downloads/Analisis_Numerico/Proyecto/datosVIH.xlsx"
# Si la funcion Leer_Datos recibe como argumento NULL, se tomara una direccion por defercto ("C:/Users/Breayann/Downloads/Analisis_Numerico/Proyecto/datosVIH.xlsx").
Leer_Datos<-function(Direc){
      if (is.null(Direc)){
      datos<<-read_excel("C:/Users/Breayann/Downloads/Analisis_Numerico/Proyecto/datosVIH.xlsx")
      }
      else{
    datos<<-read_excel(Direc)
      }
  
}



## PRESENTAR INFORME ANALIZANDO LOS DATOS INGRESADOS.

Informe=function(){
  
  # El modelo exponencial tiene la siguinete forma
  # y(x) = b * e ^(a*x)
    newdatos<-data.frame(datos[1:nrow(datos),1],log(datos[1:nrow(datos),2]))
    Regrecion <- lm(Y ~ X, data=newdatos)
    
  # Convertir a numericos los datos para procesarlos
    xpolframe=as.data.frame(datos[1])
    xpol<<-as.numeric(xpolframe[,1])
    
    ypolframe=as.data.frame(datos[2])
    yp<<-as.numeric(ypolframe[,1])
    
  # Reescalar los datos
    ypol<<-yp
    for(i in 1:length(yp)){
      ypol[i]<<-(yp[i]*(1/max(yp)))
    }
    
  # Valor de  a, que significa la probabilidad de morir por unidad de tiempo.
    a=coefficients(Regrecion)[2]
    
  # Valor de b,
    B=exp(coefficients(Regrecion)[1])
    b=exp(B)
    

  # Fraccion Inmortal
    
    Si<-min(ypol)
    

  # Funcion que ajusta los datos
    FuncionApro<<- function(t){Si+(1-Si)*exp(a*t)}
    tp<<-seq(0,max(datos[,1]),(max(datos[,1]))/100)

   ## Spline que se ajusta los datos
    Spline<<-smooth.spline(xpol,ypol)
    
    # Mayor tasa de cambio
      Cambio<<-c()
      
      for(i in 1:(length(ypol)-1)){
        
        Cambio[i]<<-abs(ypol[i]-ypol[i+1])
      }
      
      for(j in 1:length(Cambio)){ if(Cambio[j]>=max(Cambio)) {Pos<<-j} ## posicion del mayor cambio
      }
  

    ## DATOS DEL INFORME

    #Tiempo promedio de supervivencia, en semanas

      aux1=0;
      for(i in 1:length(ypol)){
        
        if(ypol[i]>=0.5){
          aux1=aux1+1
        }
      }
      Tpromedio=round(xpol[aux1]);
      

    # Supervivencia de vida media, en semanas
    
      Tvmedia=round((log(2)/abs(a))/2);
      
    # Coeficiente de correlacion
      R=as.numeric(summary(Regrecion)[8])
    
      
     # Imprimir datos del informe
      if(R<0.8){ cat("ADVERTENCIA : El coeficiente de correlacion no es muy bueno","\n","Introduzca mas datos si es posible","\n","\n")}
    
      cat("               INFORME          ","\n","\n")
      cat("Tiempo Promedio de Supervivencia = ",Tpromedio,"Semanas","\n","\n")
      cat("Supervivencia de vida media = ",Tvmedia,"Semanas","\n","\n")
      cat("Mayor tasa de Mortalidad, de la semana ",round(xpol[Pos]),"a la semana ",round(xpol[Pos+1]),"\n","\n")
      cat("Coeficiente de correlacion dado un modelo exponencial decreciente = ",round(R,3))
      
}



# GRAFICAS

Graficar=function(){

  #Datos
    plot(xpol,ypol,xlim=c(0,max(datos[,1])),ylim=c(0,1),main="Fraccion de Supervivencia", ylab="Fraccion de Supervivencia",xlab="Tiempo en semanas",col="blue",lwd=2)
    grid(10, 10, lwd = 2) # grid only in y-direction
    lines(tp,FuncionApro(tp),col="green",lwd=2)
    lines(Spline,col="red",lwd=2)
    legend("topright",col=c("blue","green","red"),legend =c("Datos Reales","Ajuste modelo exponencial","Spline"), lwd=0.7, bty = "o")
}


