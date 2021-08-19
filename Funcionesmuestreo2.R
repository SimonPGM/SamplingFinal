#Estimaciones de miu, tau e ICs 
estMu <- function(nh, yh, sigmah, Nh = NULL, Wh = NULL, aprox = T, conf = NULL, finite = T){
  #sigmah son varianzas
  #aprox si se usa 2 en el B, en caso de false especificar lvl de confianza
  #Nh nulo significa poblacion finita
  #Wh no nulo es para cuando dan los Wh y no dan los Nh
  if (!is.null(Nh)) {
    N <- sum(Nh)
  }
  
  n <- sum(nh)
  H <- length(nh)
  
  if (is.null(Wh)) {
    Wh <- Nh/N
  }
  
  finite <- rep(finite, H)
  muhat <- sum(Wh*yh)
  corr <- ifelse(finite, 1-nh/Nh, rep(1, H))
  varyh <- sum(Wh^2*corr*sigmah/nh)
  cuant <- conf + 0.5*(1-conf)
  Bmu <- ifelse(aprox,2,ifelse(n >= 30, qnorm(cuant), qt(cuant, df = n-H)))*sqrt(varyh)
  Imu <- c(muhat-Bmu, muhat+Bmu)
  list(info = data.frame(est = muhat,
                           B = Bmu, lInf = Imu[1], lSup = Imu[2]))
}

estTau <- function(nh, yh, sigmah, Nh, aprox = T, conf = NULL){
  N <- sum(Nh)
  lapply(estMu(nh, yh, sigmah, Nh, aprox = aprox,  conf = conf), function(x) N*x)
  
}

#Estimaciones de P, A e ICs 
estP <- function(nh, ph = NULL, ah = NULL, Nh = NULL, Wh = NULL, aprox = T, conf = NULL, finite = T){
  
  if (!is.null(Nh)) {
    N <- sum(Nh)
  }
  
  if (!is.null(ah)) {
    ph <- ah/nh
  }
  
  n <- sum(nh)
  H <- length(nh)
  finite <- rep(finite, H)
  corr <- ifelse(finite, 1-nh/Nh, rep(1, H))
  
  if (is.null(Wh)) {
    Wh <- Nh/N
  }
  cuant <- conf + 0.5*(1-conf)
  pest <- sum(Wh*ph)
  varph <- sum(Wh^2*corr*ph*(1-ph)/(nh-1))
  Bpest <- ifelse(aprox,2,ifelse(n >= 30, qnorm(cuant), qt(cuant, df = n-H)))*sqrt(varph)
  Intpest <- c(pest-Bpest, pest+Bpest)
  list(info = data.frame(est = pest,
                         B = Bpest, lInf = Intpest[1], lSup = Intpest[2]))
  
}

estA <- function(nh, ph = NULL, ah = NULL, Nh, aprox = T, conf = NULL){
  N <- sum(Nh)
  lapply(estP(nh, ph, ah, Nh, aprox = aprox,  conf = conf), function(x) N*x)
}

#Tamaño de muestra para estimar mu
samplesize <- function(Nh, sigmah, wh, D){
  # Sigmah son varianzas 
  num <- sum(Nh^2*sigmah/wh)
  den <- sum(Nh)^2*D+sum(Nh*sigmah)
  n <- num/den
  data.frame(naprox = n, n = ceiling(n))
}

genafij <- function(Nh, sh2 = NULL, Ch = NULL, equal = F){
  #hace cualquier caso de afijacion
  h <- length(Nh)
  if (equal){
    return(rep(1/h, h))
  }
  if (is.null(sh2)){
    sh2 <- rep(1,h)
  }
  if (is.null(Ch)){
    Ch <- rep(1,h)
  }
  Nh*sqrt(sh2/Ch)/sum(Nh*sqrt(sh2/Ch))
}

#--------------------------------POSTESTRATIFICACION----------------------------
# C?lculo ypost y taupost
Estpost <- function(Nh, yh, sh, n){
  #sh son varianzas 
  N <- sum(Nh)
  ypost <- sum((Nh/N)*yh)
  varypost <- ((N-n)/(N^2*n))*sum(Nh*sh) + (1/n^2)*sum((1-Nh/N)*sh)
  taupost <- N*ypost
  vartaupost <- N^2*varypost
  list(ypost = data.frame(ypost = ypost, varypost = varypost ),
       taupost = data.frame(taupost = taupost, vartaupost = vartaupost))
}

# Construcci?n de estratos 
Estratospost <- function(frec, H){
  # frec son las frecuencias
  # N?mero de estratos deseado
  N <- sum(frec)
  sqrfrec <- round(sqrt(frec),4)
  freccum <- cumsum(sqrfrec)
  Q <- sum(sqrfrec)/H
  divp <- c()
  aux <- matrix(ncol = H-1, nrow = length(frec))
  position <-c()
  for (i in 1:(H-1)){
    divp[i] <- i*Q
    aux[,i] <- rep(divp[i], length(frec))
    position[i] <- which.min(abs(freccum-t(aux[,i])))
  }
  Nh <- c(sum(frec[1:position[1]]))
  for (i in 2:length(position)){
    current <- position[i]
    last <- position[i-1]
    Nh[i] <- sum(frec[1:current])-sum(frec[1:last])
  }
  Nh <- append(Nh, N-sum(Nh))
  list(TablaCompleta = data.frame(Frec = frec, SqrtFrec = sqrfrec, SumSqrtFrec = freccum ),
       PosicionesDeCorte = position, Q = Q, divp = divp, Nh = Nh)
}


#---------------------------------MUESTREO DOBLE--------------------------------
# Calculo de Ymdest 
y_mdest <- function(nh_prime, nh, yh_bar, sh2){
  #B es con una confianza qproximada del 95%
  n_prime <- sum(nh_prime)
  wh_prime <- nh_prime/n_prime
  y_hat <- sum(wh_prime * yh_bar)
  var_yhat <- (n_prime/(n_prime - 1)) * sum((wh_prime^2 - (wh_prime/n_prime)) * sh2/nh + 
                                              (wh_prime * (yh_bar - y_hat)^2)/n_prime)
  B <- 2 * sqrt(var_yhat)
  IC <- c(y_hat - B, y_hat + B) #Aproximadamente del 95%
  list(y_mdest = y_hat, var_ymdest = var_yhat, B = B, IC = IC)
}

#--------------------------------SISTEMATICO LINEAL-----------------------------

#####simon#####################################
generate_sample_sis <- function(N,n, seed = NULL){
#GENERA MUESTRA ALEATORIA SISTEMATICA CUANDO N/n ES UN ENTERO
  #SEED ES EL PUNTO DE PARTIDA EN CASO DE QUE LO DEN
  k <- N/n
  r <- ifelse(is.null(seed), sample(1:k, 1), seed)
  seq(r, r+(n-1)*k, by = k)
}

A1_sample_sis <- function(N, n, new = NULL,  seed = NULL){
#GENEREA MUESTRA ALEATORIA SISTEMATICA CUANDO N/n NO ES UN ENTERO ALTERNATIVA1
#NEW ES LA MUESTRA POR SI LA DAN (PORQUE PREVIO A ESOCOGER HAY QUE REMUESTREAR)
#SEED ES EL PUNTO DE PARTIDA EN CASO DE QUE LO DEN
  k <- floor(N/n)
  if (is.null(new)){
    new <- sample(1:N, n*k) 
  }
  r <- ifelse(is.null(seed),sample(1:k, 1),seed)
  new[seq(r, r + (n-1)*k, by = k)]
}

A2_sample_sis <- function(N,n, seed = NULL){
#GENEREA MUESTRA ALEATORIA SISTEMATICA CUANDO N/n NO ES UN ENTERO ALTERNATIVA2
#SEED ES EL PUNTO DE PARTIDA EN CASO DE QUE LO DEN
  k <- floor(N/n)
  r <- c(ifelse(is.null(seed),sample(1:N, 1), seed))
  for (i in 2:n){
    r[i] <- ifelse(r[i-1]+k <= N, r[i-1]+k, (r[i-1] + k)%% N)
  }
  r
}

NotN <- function(gap, fin, rem, start = NULL){
#CUANDO NO SE CONOCE EL N PORQUE SE TIENE MUESTREO POISSON  
  
  N <- fin + rem
  nreal <- N/gap
  nint <- floor(nreal)
  if(!is.null(start)) {
    index <- seq(start, fin, by = gap)
    return(list(N = N, nreal = nreal, samplesize  = nint,
                sample = index))
  }
  list(N = N, nreal = nreal, samplesize  = nint)
}

samplesize_sis <- function(N, sigma2, D, tot = F){
#TAMA?O DE MUESTRA DADO EL LEE  
  ceiling(N*sigma2/(sigma2 + (N-1)*D))
}

#no aleatorios (no creo que pase)
meanshit <- function(yh, kp, m){
#ESTIMACION DE LAS VAINAS RELACIONADAS CON LA MEDIA CUANDO ES EL CASO FEO DE MSL  
  ysis <- mean(yh)
  varysis <- (1-m/kp)*var(yh)/m
  list(ysis = ysis, varysis = varysis)
}

numbersamples <- function(N, n, mo, yh, conf){
#RETORNA EL NUMERO DE MUESTRAS (M) QUE SE VAN A USAR DADOS N Y n0
  ybar <- mean(yh)
  s2yh <- var(yh)
  no <- n/mo
  Z <-  qnorm(conf + 0.5*(1-conf))
  num <- Z^2*s2yh*N/(no*ybar^2)
  eps2 <- (B/ybar)^2
  den <- eps2*N/no + s2yh*(Z/y)^2
  m <- num/den
  n <- n0*ceiling(m)
  list(mreal = m, m = ceiling(m), n)
}

mu <- function(data, N = NULL, aprox = T, conf = NULL) {
  muhat <- mean(data)
  n <- length(data)
  corr <- ifelse(is.null(N), 1, (1-n/N))
  varest <- var(data)*corr/n
  
  if (!is.null(conf)) {
    cuant <- conf - (1-conf)/2  
  }
  
  B <- ifelse(aprox, 2, ifelse(n >= 30, qnorm(cuant), qt(cuant, n-1)))*sqrt(varest)
  interval <- c(muhat - B, muhat + B)
  list(muhat = muhat, B = B, liminf = interval[1], limsup = interval[2])
}

tau <- function(data, N, aprox = T, conf = NULL){
  lapply(mu(data, N, aprox, conf), function(x) N*x)
}

p <- function(data, N = NULL, aprox = T, conf = NULL) {
  phat <- mean(data)
  n <- length(data)
  corr <- ifelse(is.null(N), 1, (1-n/N))
  varest <- phat*(1-phat)*corr/(n-1)
  
  if (!is.null(conf)) {
    cuant <- conf - (1-conf)/2  
  }
  
  B <- ifelse(aprox, 2, ifelse(n >= 30, qnorm(cuant), qt(cuant, n-1)))*sqrt(varest)
  interval <- c(phat - B, phat + B)
  list(phat = phat, B = B, liminf = interval[1], limsup = interval[2])
}

A <- function(data, N, aprox = T, conf = NULL) {
  lapply(p(data, N, aprox, conf), function(x) N*x)
}


#------------------------- outdated xd -----------------------------------------
#Asignaci?n que minimiza costos 
whmincosto <- function(Nh, sigmah, Ch){
  # sigmah son desviaciones
  wh <- (Nh*sigmah/sqrt(Ch))/sum(Nh*sigmah/sqrt(Ch))
  data.frame(wh = wh)
}

#Asignaci?n ?ptima (costos iguales)
whneyman <- function(Nh, sigmah){
  # sigmah son desviaciones
  wh <- Nh*sigmah/sum(Nh*sigmah)
  data.frame(wh = wh)
}

#Asignaci?n proporcional 
whprop <- function(Nh){
  wh <- Nh/sum(Nh)
  data.frame(wh = wh)
}