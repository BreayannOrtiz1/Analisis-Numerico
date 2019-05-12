
rm(list = ls())
options(digits = 6)

require(deSolve)


yini=c(X=1,Y=2)


sistema=function(t,y,parms){
  with(as.list(y),{
    
    dX = X+0.33333*Y
    dY = 5*X-4*Y
    
    list(c(dX,dY))
  })
}

# Escala del eje de las ordenadas

h=0.01;
times = seq(from = 0, to = 20, by = h)
# Solucion 

out = ode(y = yini, times = times, func = sistema,parms = NULL,method = "rk4")

# Salida

tabla = cbind(times, out[,2] )
colnames(tabla) = c("h", " x(h) ")
tabla

# funcion Exacta

out1 = ode(y = yini, times = times, func = sistema,parms = NULL)


# Representación

# x comparacion
plot(times[0:20], out[,2][0:20],xlim=c(0,0.2),ylim=c(0,2),type="o",col="blue",main = "Comparacion Exacta vs Calculada Euler mejorado ",xlab="h", ylab="x(h)",lwd=2)
lines(times[0:20],out1[,2][0:20],col="red",lwd=1)
legend("bottomright",col=c("blue","red"),legend =c("Caclulada","Exacta"), lwd=2, bty = "o")
# y comparacion 
plot(times[0:20], out[,3][0:20],xlim=c(0,.2),ylim=c(0,2),type="o",col="blue",main = "Comparacion Exacta vs Calculada Euler mejorado ",xlab="h", ylab="y(h)",lwd=2)
lines(times[0:20],out1[,3][0:20],col="red",lwd=1)
legend("bottomright",col=c("blue","red"),legend =c("Caclulada","Exacta"), lwd=2, bty = "o")



#error relativo
error1=c()
error2=c()
#error 1
for (i in 1:20){
  
  error1[i]=(abs(out1[,2][i]-out[,2][i])/out1[,2][i])
}
# error2
for (i in 1:20){
  
  error2[i]=(abs(out1[,3][i]-out[,3][i])/out1[,3][i])
}
# error global

errorg=sum(error1)+sum(error2);
print(errorg)

# Grafica del error relativo

plot(c(1:20),error1,type = "l",col="red",lwd=2,main="Error relativo x")
plot(c(1:20),error2,type = "l",col="red",lwd=2,main="Error relativo y")



