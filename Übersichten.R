#_____________________________________________________
#
# Übersichten für die Zwischenreflexion 
#
#_____________________________________________________

#Erstellung von Data-Frames
Errors <- data.frame(fall0_matrix[1,], fall1_matrix[1,], fall2_matrix[1,], fall3_matrix[1,], fall4_matrix[1,], fall5_matrix[1,], fall6_matrix[1,], fall7_matrix[1,], fall8_matrix[1,], fall9_matrix[1,])
Steps <- data.frame(fall0_matrix[3,],fall1_matrix[3,], fall2_matrix[3,], fall3_matrix[3,], fall4_matrix[3,], fall5_matrix[3,], fall6_matrix[3,], fall7_matrix[3,], fall8_matrix[3,], fall9_matrix[3,])
Thresholds <- data.frame(fall0_matrix[2,],fall1_matrix[2,], fall2_matrix[2,], fall3_matrix[2,], fall4_matrix[2,], fall5_matrix[2,], fall6_matrix[2,], fall7_matrix[2,], fall8_matrix[2,], fall9_matrix[2,])

#Zeilenbenennung
rownames(Errors) <- c("Funkt. 1" ,"Funkt. 2", "Funkt. 3", "Funkt. 4", "Funkt. 5", "Funkt. 6")
rownames(Steps) <- c("Funkt. 1" ,"Funkt. 2", "Funkt. 3", "Funkt. 4", "Funkt. 5", "Funkt. 6")
rownames(Thresholds) <- c("Funkt. 1" ,"Funkt. 2", "Funkt. 3", "Funkt. 4", "Funkt. 5", "Funkt. 6")

#Spaltenbenennung
colnames(Errors) <- c("Variation 0", "Variation 1", "Variation 2", "Variation 3", "Variation 4", "Variation 5", "Variation 6", "Variation 7", "Variation 8", "Variation 9")
colnames(Steps) <- c("Variation 0", "Variation 1", "Variation 2", "Variation 3", "Variation 4", "Variation 5", "Variation 6", "Variation 7", "Variation 8", "Variation 9") 
colnames(Thresholds) <- c("Variation 0", "Variation 1", "Variation 2", "Variation 3", "Variation 4", "Variation 5", "Variation 6", "Variation 7", "Variation 8", "Variation 9")

#LaTeX-Formatierung
xtable(t(Errors), digits = 4)
xtable(t(Steps), digits = 0)
xtable(t(Thresholds), digits = 4)
