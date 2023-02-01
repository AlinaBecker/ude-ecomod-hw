#Coding exercise 3
#4c: Population dynamics of SBM spotted owls

#formula 3.5a
#n(t+1) = Nt+r*Nt*(1-Nt/K)

r <- 0.86
K <- 266
Nt <- 380

SBM <- function(r, K, Nt){
  Nt1 <- Nt+r*Nt*(1-Nt/K)
  return(Nt1)
}

Nt1 <- SBM(r, K, Nt)
# =239.9428

N <- rep(NA, 100)

N[1] <- Nt
for (i in 1:100) {
  N[i] <- SBM(r, K, Nt)
  Nt <- N[i]
}

plot(N, xlab = "time", ylab = "N", pch = 19, col = "black")

dat <- as.data.frame(N)
dat$time <- as.numeric(rownames(dat))
library(ggplot2)
ggplot2::ggplot(dat, aes(time, N)) + geom_point()

#5

#Step 1: Build transition Matrix

P.12 <- 0.344
P.22 <- 0.767
P.32 <- 0
P.31 <- 0
P.13 <- 0
P.23 <- 0.1
P.33 <- 0.1
F1 <- 0.304
F2 <- 0.304
F3 <- 0.304

A <- matrix(c(
  F1, F2, F3,
  P.12, P.22, P.32, P.31, P.13, P.23, P.33)
  , nrow=3, ncol=7, byrow = T)

A

#Step 2: Build matrix Nt (initional abundance matrix)

Nt <- c((Nt/2), (Nt/2))

#Step 3: Matrix multiplication for Nt+1

Yearl <- A%*%Nt #matrix multiplication
Yearl

#Step 4: Create new variable to hold future values of N

N <- array(NA, dim = c(20, 2))

#Step 5: Create a for loop to iteratively calculate N

N[1, ] <- Nt
for(i in 2:20){
  N[i, ] <- A%*%N[i-1, ]
}
