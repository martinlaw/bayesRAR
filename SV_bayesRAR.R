# K <- 10
# success <- rbinom(K, 1, prob=0.3)
# failure <- -success+1
# s <- cumsum(success)
# f <- cumsum(failure)
# 
nsims <- 10000
# b.list <- vector("list", K)
# 
# for (k in 1:K) {
#   b.list[[k]] <- rbeta(nsims, s[k], f[k])
# }
# 
# b <- do.call(cbind, b.list)

K <- 2 # no. arms
s <- c(3,8) # successes on each arm
f <- c(7,2) # failures on each arm
b <- matrix(NA, nrow=nsims, ncol=K)

for(k in 1:K){ # for each arm k,
  b[, k] <- rbeta(nsims, s[k], f[k])
}

B <- apply(b, 1, max)
bmax <- apply(b, 1, which.max)



n <- sum(s,f)+1
N <- 40
const <- (n-1)/N
# allocation ratio:
elp <- NULL
for (k in 1:K) {
 # c <- (t + 1) / (2 * Mh)
  elp[k] <- (sum(bmax == k) / nsims)^const
}
