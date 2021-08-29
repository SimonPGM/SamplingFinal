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

Nh <- c(32 ,38, 32)
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
nh <- round(wh.1*nfinal) + c(1, 0, 0)

sizes <- data.frame(Nh = Nh, nh = nh) %>%
  mutate(Porcentaje = paste(round(100*nh/Nh), rep("%", 2), sep = ""))
rownames(sizes) <- c("Noveno", "Décimo", "Once")

sh2s <- matrix(c(sh2.1, sh2.2, sh2.3, sh2.4, sh2.6), nrow = 3) %>%
  as.data.frame() %>%
  round(4)
colnames(sh2s) <- paste(rep("Pregunta", 5), as.character(c(1, 2, 3, 4, 6)))
rownames(sh2s) <- c("Noveno", "Décimo", "Undécimo")

whs <- matrix(c(wh.1, wh.2, wh.3, wh.4, wh.6), nrow = 3) %>% 
  as.data.frame() %>%
  round(4)
colnames(whs) <- paste(rep("Pregunta", 5), as.character(c(1, 2, 3, 4, 6)))
rownames(whs) <- c("Noveno", "Décimo", "Undécimo")

ns <- data.frame(p1 = n.1, p2 = n.2, p3 = n.3, p4 = n.4, p6 = n.6)


# write_csv(sizes, "StrataGlobalnSampleSizes.csv")
# write_csv(sh2s, "PilotSampleVariances.csv")
# write_csv(whs, "ProportionsofStrata.csv")
# write_csv(ns, "EstimatedSampleSizes.csv")

set.seed(123)
undecimo <- sample(1:Nh[3], 24)
set.seed(471)
decimo <- sample(1:Nh[2], 28)
set.seed(621)
noveno <- sample(1:Nh[1], 47)
dfnoveno <- data.frame(numero = noveno) %>%
  mutate(grupo = if_else(numero <= 32, "B", "A"),
         grupo = factor(grupo, levels = c("B", "A")),
         numero = if_else(grupo == "B", numero, as.integer(numero %% 32)))
novenoa <- dfnoveno[dfnoveno$grupo == "A",1]
novenob <- dfnoveno[dfnoveno$grupo == "B",1]

#depuracion base completa
full <- read.csv("Respuestascomp.csv", header = T)[-c(89, 90), ]

full <- full %>%
  arrange(Marca.temporal)

full <- full[-c(2, 6,7),]

full <- full %>%
  arrange(Dirección.de.correo.electrónico)

full <- full[-c(37, 45, 55, 68), ]
full[74, 3] <- "Decimo"
full <- full[, -c(1, 2, 8)]
rownames(full) <- as.character(1:81)
colnames(full) <- c("Grado", paste("Pregunta_", c(1:4, 6), sep = ""))
write.csv(full, "FixedFullDataBase.csv", row.names = F)
