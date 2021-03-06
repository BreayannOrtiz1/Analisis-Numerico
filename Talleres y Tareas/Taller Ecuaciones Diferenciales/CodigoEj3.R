rm(list = ls())
options(digits = 6)

require(deSolve)


yini=c(X=1,Z=2)


sistema=function(x,y,parms){
  with(as.list(y),{
    
    dZ = y[2]+x[1]-y[1]-1
    dY = y[2]
    
    list(c(dY,dZ))
  })
}
# Escala del eje de las ordenadas

h=0.01;
times = seq(from = 0, to = 20, by = h)
# Solucion 

out = ode(y = yini, times = times, func = sistema,parms = NULL,method = "rk4")

# Salida

tabla = cbind(times, out[,2] )
colnames(tabla) = c("h", " y(h) ")
tabla
# "Funcion Exacta"
out1 = ode(y = yini, times = times, func = sistema,parms = NULL)

plot(times[0:20], out[,2][0:20],xlim=c(0,0.2),ylim=c(0,2),type="o",col="blue",main = "Comparacion Exacta vs Calculada ",xlab="h", ylab="x(h)",lwd=1)
lines(times[0:20],out1[,2][0:20],col="red",lwd=1)
legend("bottomright",col=c("blue","red"),legend =c("Caclulada","Exacta"), lwd=2, bty = "o")

#error relativo
error=c()
for (i in 1:20){
  
  error[i]=(abs(out1[,2][i]-out[,2][i])/out1[,2][i])*100
}

# error global

errorg=sum(error);
print(errorg)

# Grafica del error relativo

plot(c(1:20),error,type = "l",col="red",lwd=2,main="Error relativo")



