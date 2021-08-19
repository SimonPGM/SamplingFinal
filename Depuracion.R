library(tidyverse)
source("Funcionesmuestreo2.R")
cdata <- read.csv("DatosMuestraPiloto.csv", header = T)[,c(-1,-2)]
colnames(cdata) <- c("Grado", paste(rep("Pregunta_", 6), as.character(1:6), sep = ""))

corrected <- cdata
n <- dim(corrected)[1]
for (i in 1:n) {
  corrected[i,3] <- str_extract_all(corrected[i, 3], "\\d{1,2}")[[1]] %>%
    as.numeric() %>%
    mean()
  
  corrected[i, 5] <- str_extract_all(corrected[i, 5], "\\d{1,2}")[[1]] %>%
    as.numeric() %>%
    mean() 
}
corrected[n,3] <- 0
corrected[10,3] <- min(corrected[-n,3])
corrected <- corrected %>%
  mutate(Pregunta_2 = as.numeric(Pregunta_2),
         Pregunta_2 = if_else(Pregunta_2 >= 30, Pregunta_2 -30, Pregunta_2),
         Pregunta_4 = as.numeric(Pregunta_4),
         Pregunta_1 = if_else(Pregunta_1 == "Satisfecho", 1, 0),
         Pregunta_3 = if_else(Pregunta_3 == "Si", 1, 0),
         Pregunta_6 = if_else(Pregunta_6 == "Si", 1, 0))

corrected[n, 5] <- 0
corrected <- corrected[-5, ] %>%
  select(-Pregunta_5)
  

noveno <- corrected %>%
  filter(Grado == "Noveno")

decimo <- corrected %>%
  filter(Grado == "Decimo")

once <- corrected %>%
  filter(Grado == "Undecimo")

ldf <- list(noveno, decimo, once)

Nh <- c(32 + 31,38, 32)
#Pregunta 1
sh2.1 <- c()
for (i in 1:3) {
  temp <- ldf[[i]]
  
  sh2.1[i] <- mean(temp[,2])*(1-mean(temp[,2]))
}
wh.1 <- genafij(Nh, sh2.1)
D.1 <- (0.05/qnorm(0.975))^2
ns.1 <- samplesize(Nh, sh2.1, wh.1, D.1)
n.1 <- ns.1[[2]]

#Pregunta 2
sh2.2 <- c()
for (i in 1:3) {
  temp <- ldf[[i]]
  
  sh2.2[i] <- var(temp[,3])
}
wh.2 <- genafij(Nh, sh2.2)
D.2 <- (2/qnorm(0.975))^2
ns.2 <- samplesize(Nh, sh2.2, wh.2, D.2)
n.2 <- ns.2[[2]]

#Pregunta3
sh2.3 <- c()
for (i in 1:3) {
  temp <- ldf[[i]]
  
  sh2.3[i] <- mean(temp[,4])*(1-mean(temp[,4]))
}
wh.3 <- genafij(Nh, sh2.3)
D.3 <- (0.05/qnorm(0.975))^2
ns.3 <- samplesize(Nh, sh2.3, wh.3, D.3)
n.3 <- ns.3[[2]]

#Pregunta 4
sh2.4 <- c()
for (i in 1:3) {
  temp <- ldf[[i]]
  
  sh2.4[i] <- var(temp[,5])
}

wh.4 <- genafij(Nh, sh2.4)
D.4 <- (2/qnorm(0.975))^2
ns.4 <- samplesize(Nh, sh2.4, wh.4, D.4)
n.4 <- ns.4[[2]]


#pregunta 6
sh2.6 <- c()
for (i in 1:3) {
  temp <- ldf[[i]]
  
  sh2.6[i] <- mean(temp[,6])*(1-mean(temp[,6]))
}
wh.6 <- genafij(Nh, sh2.6)
D.6 <- (0.05/qnorm(0.975))^2
ns.6 <- samplesize(Nh, sh2.6, wh.6, D.6)
n.6 <- ns.6[[2]]

nfinal <- n.1
nh <- round(wh.1*nfinal)