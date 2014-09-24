# perplexity

N<-4
P1 <- 1/4
P2 <- 1/4
P3 <- 1/4
P4 <- 1/120000

SP <- P1*P2*P3*P4
SP**(-1/N)

lP1 <- log(P1)
lP2 <- log(P2)
lP3 <- log(P3)
lP4 <- log(P4)

lp <- lP1+lP2+lP3+lP4

exp((-1/N)*(lp))

# vecotorize
lpv <- c(lP1,lP2,lP3,lP4)

perplexity <- function(logP,N){
    # wants a vector of logP
    return(exp((-1/N)*sum(logP)))
}
perplexity(lpv,4)
