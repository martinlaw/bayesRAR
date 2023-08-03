nsims <- 100
K <- 2 # no. arms
s <- c(3,4) # successes on each arm
f <- c(7,6) # failures on each arm
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
  elp[k] <- (sum(bmax == k) / nsims)^const
}
