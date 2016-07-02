

#_____________________________________________________
#
# Simulationsstudie zu den Approximations-Fähigkeiten eines Neuronalen Netzes in R 
#
#_____________________________________________________


library(neuralnet)
library(xtable)
library(knitr)

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 
# 1. replication of Bishop's approximations and some more (p. 248)
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ 

# create data
set.seed(10) #Reproduzierbares Experiment 

n1 <-50  #Bishop's Konfiguration

a1 <-runif(n1,min = -1, max = 1)
a2<-rnorm(n1)
a3 <-rt(n1,2)
a4 <-rt(n1,5)

#Funktion 1: Parabel
f_1 <- function(x){
  y <- x^2
  return(y)
}   

#Funktion 2: Sinus
f_2 <- function(x){
  y <- sin(2*x)
  return(y)
} 

#Funktion 3: Betragsfunktion
f_3 <- function(x){
  y <- abs(x)
  return(y)
}  

#Funktion 4: Heavyside-Funktion
f_4 <-function(x){
  y<-as.numeric(x>0)
  return(y)
}

#Funktion 5: Komposition
f_5 <- function(x){
  y <- 2*exp(x^2)+2*x^2*cos(pi/(2*x^3))
  return(y)
}  

#Funktion 6: Komposition
f_6 <- function(x){
  y<-x-x^2*floor(1/x)
  return(y)
}


#Wertetabellen als Data Frame mit n1 = 50
dat1 <- data.frame(a1,f_1(a1))
names(dat1) <- c("x1","y1")

dat2 <- data.frame(a1,f_2(a1))
names(dat2) <- c("x2","y2")

dat3 <- data.frame(a1,f_3(a1))
names(dat3) <- c("x3","y3")

dat4 <- data.frame(a1,f_4(a1))
names(dat4) <- c("x4","y4")

dat5 <- data.frame(a1,f_5(a1))
names(dat5) <- c("x5","y5")

dat6 <- data.frame(a1,f_6(a1))
names(dat6) <- c("x6", "y6")

#Trainieren des Neuronalen Netzes
nn1 <- neuralnet(y1 ~ x1, data = dat1, hidden = c(2,2), rep=1, err.fct = "sse")
plot(nn1)

nn2 <- neuralnet(y2 ~ x2, data = dat2, hidden = c(2,2), rep=1, err.fct = "sse")
plot(nn2)

nn3 <- neuralnet(y3 ~ x3, data = dat3, hidden = c(2,2), rep=1, err.fct = "sse")
plot(nn3)

nn4 <- neuralnet(y4 ~ x4, data = dat4, hidden = c(2,2), rep=1, stepmax = 1e+08)
plot(nn4)

nn5 <- neuralnet(y5 ~ x5, data = dat5, hidden = c(2,2), rep=1, stepmax = 1e+08)
plot(nn5)

nn6 <- neuralnet(y6 ~ x6, data = dat6, hidden = c(2,2), rep=1, stepmax = 1e+08)
plot(nn6)


#Berechnung der Fehler
e_1<- round(nn1$result.matrix[1,],5)
e_2<- round(nn2$result.matrix[1,],5)
e_3<- round(nn3$result.matrix[1,],5)
e_4<- round(nn4$result.matrix[1,],5)
e_5<- round(nn5$result.matrix[1,],5)
e_6<- round(nn6$result.matrix[1,],5)

#Approximation der Funktionswerte
f_1_approx <- approxfun(x=dat1[,1], y = nn1$response, method = "linear")
f_2_approx <- approxfun(x=dat2[,1], y = nn2$response, method = "linear")
f_3_approx <- approxfun(x=dat3[,1], y = nn3$response, method = "linear")
f_4_approx <- approxfun(x=dat4[,1], y = nn4$response, method = "linear")
f_5_approx <- approxfun(x=dat5[,1], y = nn5$response, method = "linear")
f_6_approx <- approxfun(x=dat6[,1], y = nn6$response, method = "linear")

#Plot 1
curve(f_1, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=x^2")
curve(f_1_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat1[,1], y = nn1$response, col="blue", lty=2, pch=16, cex= 1)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_1), border="white")

#Plot 2
curve(f_2, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=sin(2x)")
curve(f_2_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat2[,1], y = nn2$response, col="blue", lty = 2, pch = 16)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_2),border="white")

#Plot 3
curve(f_3, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=|x|")
curve(f_3_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat3[,1], y = nn3$response, col="blue", lty=2, pch = 16)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_3),border="white")

#Plot 4
curve(f_4, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=25*sin(10*x)^2 - 15)*exp(-2*x)")
curve(f_4_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat4[,1], y = nn4$response, col="blue", lty=2, pch = 16)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_4),border="white")

#Plot 5
curve(f_5, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=exp(x^2)+x^2*cos(pi/(2*x^3))")
curve(f_5_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat5[,1], y = nn5$response, col="blue", lty=2, pch = 16)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_5),border="white")

#Plot 6
curve(f_6, from=-1, to=1, n=1000,col="red", ylab="")#, main="(a) f(x)=exp(x^2)+x^2*cos(pi/(2*x^3))")
curve(f_6_approx, from=-1, to=1, n=1000, col="blue", add=TRUE)
points(dat6[,1], y = nn6$response, col="blue", lty=2, pch = 16)
legend("topright", legend=c("true","nn"), lty=c(1,1), col=c("red","blue"))
legend("topleft", legend=paste0("Error: ", e_6),border="white")

#Gesamtergebis-Matrix
fall0_matrix <-  cbind(nn1$result.matrix,nn2$result.matrix,nn3$result.matrix,nn4$result.matrix,nn5$result.matrix,nn6$result.matrix)

#Fehlertabelle
Error <- fall0_matrix[1,]
fehlermatrix <-matrix(Error, nrow = 6, byrow = TRUE)
#colnames(fehlermatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(fehlermatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3", "Funktion 4", "Funktion 5", "Funktion 6")
xtable(fehlermatrix, digits = 5)

#Schrittanzahl-Tabelle
Steps <- fall0_matrix[3,]
stepmatrix <-matrix(Steps, nrow = 6, byrow = TRUE)
#colnames(stepmatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(stepmatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3", "Funktion 4", "Funktion 5", "Funktion 6")
xtable(stepmatrix, digits = 5)

#Schwellenwert-Tabelle
Threshold <- fall0_matrix[2,]
thresholdmatrix <-matrix(Threshold, nrow = 6, byrow = TRUE)
#colnames(thresholdmatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(thresholdmatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3", "Funktion 4", "Funktion 5", "Funktion 6")
xtable(thresholdmatrix, digits = 5)

#Übersicht
overview <- cbind(Error, Steps, Threshold)
rownames(overview) <- c("Funktion 1" ,"Funktion 2", "Funktion 3", "Funktion 4", "Funktion 5", "Funktion 6")
xtable(overview, digits = 5)


