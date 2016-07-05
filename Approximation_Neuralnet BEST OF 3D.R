#_____________________________________________________
#
# Simulationsstudie zu den Approximations-Fähigkeiten eines Neuronalen Netzes in R 
#
#_____________________________________________________

#' ---
#' author: "Theresa Blümlein"
#' date: "03.07.2016"
#' ---

library(neuralnet)
library(xtable)
library(knitr)

# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
# Funktionsapproximationen mit möglichst güstigen Parametern für jede Funktion 
# _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _

# create data
set.seed(10) #Reproduzierbares Experiment 

n1 <-50 #Bishop's Konfiguration
n2 <-500 #Variation 4

a1 <-runif(n1,min = -1, max = 1)
a2 <-rnorm(n1)
a3 <-rt(n1,2) 

b1 <-runif(n2,min = -1, max = 1)
b2 <-rnorm(n2)
b3 <-rt(n2,2)

#Funktion 1: Sphere-Funktion
sph <- function(x1,x2){
  z <- x1^2+x2^2
  return(z)
}   

#Funktion 2: Eggholder-Funktion
egg <- function(x1,x2){
  y <- -(x2+47) * sin(sqrt(abs(x2+x1/2+47))) + -x1 * sin(sqrt(abs(x1-(x2+47))))
  return(y)
} 

#Funktion 3: Cross-In-Tray-Funktion
cit <- function(x1,x2){
  y <- -0.0001 * (abs(sin(x1)*sin(x2)*exp(abs(100 - sqrt(x1^2+x2^2)/pi)))+1)^0.1
  return(y)
}  



#Wertetabellen als Data Frame
dat1 <- data.frame(a2,a2,sph(a2,a2))
names(dat1) <- c("x1sph","x2sph", "ysph")

dat2 <- data.frame(a2,a2,egg(a2,a2))
names(dat2) <- c("x1egg","x2egg", "yegg")

dat3 <- data.frame(a2,a2,cit(a2,a2)) 
names(dat3) <- c("x1cit", "x2cit","ycit")



##Lernraten
#dramatisiert
minus7 = 0.001 
plus7 = 100

#angeglichen
minus8 = 0.5
plus8 = 1.5


#Trainieren der Neuronalen Netze
nn1 <- neuralnet(ysph ~ x1sph + x2sph, data = dat1, hidden = c(10,10), rep=1, learningrate.factor = list(minus8, plus8), stepmax = 1e+07)
nn2 <- neuralnet(yegg ~ x1egg + x2egg, data = dat2, hidden = c(10,10,10,10), rep=1, learningrate.factor = list(minus7, plus7), stepmax = 1e+07)
nn3 <- neuralnet(ycit ~ x1cit + x2cit, data = dat3, hidden = c(10,10), rep=1, learningrate.factor = list(minus8, plus8), stepmax = 1e+07)


#Plots der Neuronalen Netze
plot(nn1, information = FALSE, show.weights = FALSE)
plot(nn2, information = FALSE, show.weights = FALSE)
plot(nn3, information = FALSE, show.weights = FALSE)


#Berechnung der Fehler
e_1<- round(nn1$result.matrix[1,],5)
e_2<- round(nn2$result.matrix[1,],5)
e_3<- round(nn3$result.matrix[1,],5)


#Gesamtergebis-Matrix
fallbo3d_matrix <-  cbind(nn1$result.matrix[1:3,],nn2$result.matrix[1:3,],nn3$result.matrix[1:3,])

#Fehlertabelle
Error <- fallbo3d_matrix[1,]
fehlermatrix <-matrix(Error, nrow = 3, byrow = TRUE)
#colnames(fehlermatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(fehlermatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3")
xtable(fehlermatrix, digits = 5)

#Schrittanzahl-Tabelle
Steps <- fallbo3d_matrix[3,]
stepmatrix <-matrix(Steps, nrow = 3, byrow = TRUE)
#colnames(stepmatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(stepmatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3")
xtable(stepmatrix, digits = 5)

#Schwellenwert-Tabelle
Threshold <- fallbo3d_matrix[2,]
thresholdmatrix <-matrix(Threshold, nrow =3, byrow = TRUE)
#colnames(thresholdmatrix) <- c("Iteration 1", "Iteration 2", "Iteration 3")
rownames(thresholdmatrix) <- c("Funktion 1" ,"Funktion 2", "Funktion 3")
xtable(thresholdmatrix, digits = 5)

#Übersicht
overview <- cbind(Error, Steps, Threshold)
rownames(overview) <- c("Funktion 1" ,"Funktion 2", "Funktion 3")
xtable(overview, digits = c(0,4,0,4))


