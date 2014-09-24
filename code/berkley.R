# berkley calculation
N <-NA
N[1] <- 2081496
N[2] <- 5315
N[3] <- 1419
N[4] <- 642  
N[5] <- 381
N[6] <- 311
N[7] <- 196
# 9420 bigram presents N0 is V*V - 9420
V <- 1446
n <- function(x) N[x+1]
n(0)



c0 <- (1)* (N1/N0)
c <- 1
k <- 4
cstar <- function(c)  (c+1)*(n(c+1)/n(c))
for(i in 1:6) print(cstar(i))


# with k, not used in the paper
cstar <- ((c+1)*(n(c+1)/n(c)) - c*(((k+1)*n(k+1))/n(1))) / (1 - (((k+1)*n(k+1)) /n(1)))
