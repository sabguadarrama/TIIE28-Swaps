#### Limpiamos el área de trabajo
rm(list=ls())

#### Cargamos las librerías necesarias
library(imputeTS)
library(forecast)
#### Ejemplo de filtro de Kalman: Cargamos los datos 'presidents' y observamos que hay datos faltantes

plot(presidents)

# Imputaremos los datos faltantes usando la función na.kalman
?na.kalman

# Usamos a opción default de na.kalman (Esta opción usa la representación del estado-espacio de un modelo
# arima. Para esto, na.kalman usa la función auto.arima, que estima el mejor modelo arima de una serie 
# univariada)

imp <- na.kalman(presidents)

# Visualizamos la serie original con la imputada
# (Perform imputation with KalmanSmoother and state space representation of arima model)
plotNA.imputations(presidents,imp)
# Perform imputation with KalmanSmoother and state space representation of arima model

usermodel <- arima(presidents,order = c(1,0,1))$model
na.kalman(presidents,model = usermodel)

### Fin de ejemplo
### Ahora realizaremos la imputación con los datos de las tasas de interés

# rate<-read.csv("C:/Users/End User/Desktop/Econometría/Econometría de Series de Tiempo/Granger causality/Datos TIIE y TF.csv")
rate<-read.csv("C:/Users/ruths/Documents/Tesis/Datos TIIE y TF.csv")

# Cambiamos los "0" por na
rate[rate==0]<-NA

# Hacemos unas gráfica para ver los NA
# Gráfica 1
par(mfrow=c(2,3)); Time = 1:882

plot(Time, rate$TIIE28,type = "l", main='TIIE')
plot(Time, rate$D_32_92,type = "l",main='De 32 días a 92 días')
plot(Time, rate$D_93_184,type = "l", main='De 93 días a 184 días')

plot(Time, rate$D_185_366,type = "l", main='De 185 días a 366 días')
plot(Time, rate$D_367_731,type = "l",main='De 367 días a 731 días')
plot(Time, rate$D_732_1096,type = "l", main='De 732 días a 1,096 días')

# Gráfica 2
par(mfrow=c(2,3)); Time = 1:882

plot(Time, rate$D_1097_1461,type = "l", main='De 1,097 días a 1,461 días')
plot(Time, rate$D_1462_1827,type = "l",main='De 1,462 días a 1,827 días')
plot(Time, rate$D_1828_2557,type = "l", main='De 1,828 días a 2,557 días')

plot(Time, rate$D_2558_3653,type = "l", main='De 2,558 días a 3,653 días')
plot(Time, rate$D_3654_5479,type = "l",main='De 3,654 días a 5,479 días')
plot(Time, rate$D_5480_7305,type = "l", main='De 5,480 días a 7,305 días')

## Imputaremos los NA con el filtro de Kalman 

# De 32 días a 92 días 
imp_32_92 <- na.kalman(rate$D_32_92)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_32_92,imp_32_92,main ='De 32 días a 92 días')

# De 93 días a 184 días 
imp_93_184 <- na.kalman(rate$D_93_184)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_93_184,imp_93_184,main ='De 93 días a 184 días')

# De 185 días a 366 días
imp_185_366 <- na.kalman(rate$D_185_366)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_185_366,imp_185_366,main ='De 185 días a 366 días')

# De 367 días a 731 días
imp_367_731 <- na.kalman(rate$D_367_731)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_367_731,imp_367_731,main ='De 367 días a 731 días')

# De 732 días a 1,096 días
imp_732_1096 <- na.kalman(rate$D_732_1096)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_732_1096,imp_732_1096,main ='De 732 días a 1,096 días')

# De 1,097 días a 1,461 días
imp_1097_1461 <- na.kalman(rate$D_1097_1461)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_1097_1461,imp_1097_1461,main ='De 1,097 días a 1,461 días')

# De 1,462 días a 1,827 días
imp_1462_1827 <- na.kalman(rate$D_1462_1827)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_1462_1827,imp_1462_1827,main ='De 1,462 días a 1,827 días')

# De 1,828 días a 2,557 días
imp_1828_2557 <- na.kalman(rate$D_1828_2557)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_1828_2557,imp_1828_2557,main ='De 1,828 días a 2,557 días')

# De 2,558 días a 3,653 días
imp_2558_3653 <- na.kalman(rate$D_2558_3653)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_2558_3653,imp_2558_3653,main ='De 2,558 días a 3,653 días')

# De 3,654 días a 5,479 días
imp_3654_5479 <- na.kalman(rate$D_3654_5479)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_3654_5479,imp_3654_5479,main ='De 3,654 días a 5,479 días')

# De 5,480 días a 7,305 días
imp_5480_7305 <- na.kalman(rate$D_5480_7305)
# Visualizamos la serie original con la imputada
plotNA.imputations(rate$D_5480_7305,imp_5480_7305,main ='De 5,480 días a 7,305 días')

### Generamos un data frame con los datos imputados

rate_imp<-data.frame(date=rate$date,tiie=rate$TIIE28 ,imp_32_92,imp_93_184,imp_185_366,imp_367_731,imp_732_1096,imp_1097_1461,
                     imp_1462_1827,imp_1828_2557,imp_2558_3653,imp_3654_5479)

# Debido a la relación de datos existentes e imputados en los últimos dos periodos, se decide no ocuparlos
# para la causalidad de granger

# Procedemos con la causalidad de granger
# Load library

library(readxl)
library(zoo)
library(dynlm)
library(lmtest)
library(sandwich)
library(vars)
library(xtable)
library(car)

# Convertir las series en estacionarias
d_tiie28=diff(rate$TIIE28)
d_imp_32_92=diff(imp_32_92)
d_imp_93_184=diff(imp_93_184)
d_imp_185_366=diff(imp_185_366)
d_imp_367_731=diff(imp_367_731)
d_imp_732_1096=diff(imp_732_1096)
d_imp_1097_1461=diff(imp_1097_1461)
d_imp_1462_1827=diff(imp_1462_1827)
d_imp_1828_2557=diff(imp_1828_2557)
d_imp_2558_3653=diff(imp_2558_3653)
d_imp_3654_5479=diff(imp_3654_5479)

# Juntar serie TIIE28 con cada periodo individualmente
tiiey32=cbind(d_tiie28,d_imp_32_92)
tiiey93=cbind(d_tiie28,d_imp_93_184)
tiiey185=cbind(d_tiie28,d_imp_185_366)
tiiey367=cbind(d_tiie28,d_imp_367_731)
tiiey732=cbind(d_tiie28,d_imp_732_1096)
tiiey1097=cbind(d_tiie28,d_imp_1097_1461)
tiiey1462=cbind(d_tiie28,d_imp_1462_1827)
tiiey1828=cbind(d_tiie28,d_imp_1828_2557)
tiiey2558=cbind(d_tiie28,d_imp_2558_3653)

# Modelo var, con lag 1:5, se supone que la causalidad es sensible a los retrasos
# Primero se hacen los vectores
p32_92<-vector()
pt32_92<-vector()
p32_92_VAR<-vector()
p93_184<-vector()
pt93_184<-vector()
p93_184_VAR<-vector()
p185_366<-vector()
pt185_366<-vector()
p185_366_VAR<-vector()
p367_731<-vector()
pt367_731<-vector()
p367_731_VAR<-vector()

p732_1096<-vector()
pt732_1096<-vector()
p732_1096_VAR<-vector()
p1097_1461<-vector()
pt1097_1461<-vector()
p1097_1461_VAR<-vector()
p1462_1827<-vector()
pt1462_1827<-vector()
p1462_1827_VAR<-vector()
p1828_2557<-vector()
pt1828_2557<-vector()
p1828_2557_VAR<-vector()
p2558_3653<-vector()
pt2558_3653<-vector()
p2558_3653_VAR<-vector()

# Modelo VAR, causalidad 1 tiene hipotesis nula que el plazo no causa a la tiie28, la causalidad 2 tiene la hipotesis
# nula que la tiie28 no causa la tasa del plazo
for (i in 1:5) {
  p32_92_VAR<-VAR(tiiey32,p=i,type = "const")
  p32_92[i]<-causality(p32_92_VAR,cause = "d_imp_32_92")$Granger$p.value
  pt32_92[i]<-causality(p32_92_VAR, cause = "d_tiie28")$Granger$p.value
  p93_184_VAR<-VAR(tiiey93,p=i,type = "const")
  p93_184[i]<-causality(p93_184_VAR,cause = "d_imp_93_184")$Granger$p.value
  pt93_184[i]<-causality(p93_184_VAR, cause = "d_tiie28")$Granger$p.value
  p185_366_VAR<-VAR(tiiey185,p=i,type = "const")
  p185_366[i]<-causality(p185_366_VAR,cause = "d_imp_185_366")$Granger$p.value
  pt185_366[i]<-causality(p185_366_VAR, cause = "d_tiie28")$Granger$p.value
  p367_731_VAR<-VAR(tiiey367,p=i,type = "const")
  p367_731[i]<-causality(p367_731_VAR,cause = "d_imp_367_731")$Granger$p.value
  pt367_731[i]<-causality(p367_731_VAR, cause = "d_tiie28")$Granger$p.value
  p732_1096_VAR<-VAR(tiiey732,p=i,type = "const")
  p732_1096[i]<-causality(p732_1096_VAR,cause = "d_imp_732_1096")$Granger$p.value
  pt732_1096[i]<-causality(p732_1096_VAR, cause = "d_tiie28")$Granger$p.value
  
  p1097_1461_VAR<-VAR(tiiey1097,p=i,type = "const")
  p1097_1461[i]<-causality(p1097_1461_VAR,cause = "d_imp_1097_1461")$Granger$p.value
  pt1097_1461[i]<-causality(p1097_1461_VAR, cause = "d_tiie28")$Granger$p.value
  p1462_1827_VAR<-VAR(tiiey1462,p=i,type = "const")
  p1462_1827[i]<-causality(p1462_1827_VAR,cause = "d_imp_1462_1827")$Granger$p.value
  pt1462_1827[i]<-causality(p1462_1827_VAR, cause = "d_tiie28")$Granger$p.value
  p1828_2557_VAR<-VAR(tiiey1828,p=i,type = "const")
  p1828_2557[i]<-causality(p1828_2557_VAR,cause = "d_imp_1828_2557")$Granger$p.value
  pt1828_2557[i]<-causality(p1828_2557_VAR, cause = "d_tiie28")$Granger$p.value
  p2558_3653_VAR<-VAR(tiiey2558,p=i,type = "const")
  p2558_3653[i]<-causality(p2558_3653_VAR,cause = "d_imp_2558_3653")$Granger$p.value
  pt2558_3653[i]<-causality(p2558_3653_VAR, cause = "d_tiie28")$Granger$p.value
}

resumenpat<-data.frame(n_lag=(1:5),p32_92=p32_92,p185_366=p185_366,p367_731=p367_731,p732_1096=p732_1096,
                       p1097_1461=p1097_1461,p1462_1827=p1462_1827,p1828_2557=p1828_2557,p2558_3653=p2558_3653)

resumentap<-data.frame(n_lag=(1:5),p32_92=pt32_92,p185_366=pt185_366,p367_731=pt367_731,p732_1096=pt732_1096,
                       p1097_1461=pt1097_1461,p1462_1827=pt1462_1827,p1828_2557=pt1828_2557,p2558_3653=pt2558_3653)

write.csv(resumenpat,file = "C:/Users/ruths/Documents/Tesis/Resumen.csv")
write.csv(resumentap,file = "C:/Users/ruths/Documents/Tesis/ResumenTAP.csv")

# Para tener un modelo con los retrasos optimos definidos por AIC
# seriesdts_VAR=VAR(seriesdts, type="const", lag.max = 10, ic="AIC")

# Individualmente sería
# tiiey32_VAR=VAR(tiiey32,p=2,type="const")
# p32_92<-causality(tiiey32_VAR, cause = "d_imp_32_92")$Granger

# Se hace el test de causalidad probando lo contrario, con hipotesis nula de que la tiie28 no causa la TF plazo
causality(tiiey32_VAR, cause = "d_tiie28")$Granger