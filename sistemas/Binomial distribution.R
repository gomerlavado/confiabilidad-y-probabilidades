# Example 3.1
n <- 5
p <- 1/2
q <- 1 - p

prob <- function (p,r,n) {
  comb <- factorial(n)/(factorial(r)*factorial(n-r))
  p_r <- comb * p^r * (1-p)^(n-r)
  return(p_r)
}

# coefficient (r+1)th represents the number of ways
# in which exactly r failures and (n-r) successes can occur
# in n trials and is equal to n_C_r

prob(1/2,0,5)

mapply(prob, 0.5,seq(0,5,1),5)

library(tidyverse)
library(dslabs)

df <- data.frame(heads = seq(0,n,1), 
           tails = n - seq(0,n,1), 
           prob = mapply(prob, p,seq(0,n,1),n)
           )

df[,"cum_prob"] <- cumsum(df$prob)
df

barplot(df$prob*32,
        xlab = "Number of heads", ylab = "Probability x 32", 
        col = c("royalblue"),
        )

barplot(df$cum_prob*32,
        xlab = "Number of heads", ylab = "Probability x 32", 
        col = c("royalblue"),
        )

# Example 3.2
n <- 4
p <- 1/4
q <- 1 - p

df <- data.frame(heads = seq(0,n,1), 
                 tails = n - seq(0,n,1), 
                 prob = mapply(prob, p,seq(0,n,1),n)
)

df[,"cum_prob"] <- cumsum(df$prob)
df

barplot(df$prob*256,
        xlab = "Number of heads", ylab = "Probability x 32", 
        col = c("darkred"),
)

barplot(df$cum_prob*256,
        xlab = "Number of heads", ylab = "Probability x 32", 
        col = c("darkred"),
)


# Example 3.4
n <- 4
p <- 0.1
q <- 1 - p

df <- data.frame(defects = seq(0,n), 
                 ind_prob = mapply(prob, p,seq(0,n,1),n),
                 ex = seq(0,n) * mapply(prob, p,seq(0,n,1),n),
                 ex2 = seq(0,n)^2 * mapply(prob, p,seq(0,n,1),n)
)

df %>% summarize(ind_prob = sum(ind_prob), ex = sum(ex), ex2 = sum(ex2))

ex <- n*p
sigma <- sqrt(n*p*q)
ex
sigma

# Example 3.5
n <- 50
p <- 0.01
q <- 1 - p

prob(p,2,n) + prob(p,1,n) + prob(p,0,n)

# Example 3.6
cost <- 10
sale <- 15

sale - cost * (1+p)

p <- 0.001
cost <- 10.05
sale <- 15

sale - cost * (1+p)

# Example 3.7
n <- 4
p <- 0.9
q <- 1 - p

df <- data.frame(system_state = c("all work","1 failed","2 failed","3 failed","all fail"), 
                 ind_prob = mapply(prob, p,seq(n,0,-1),n)
)
df

P <- prob(p,4,n) + prob(p,3,n)
Q <- prob(p,0,n) + prob(p,1,n) + prob(p,2,n)
paste(P,Q, 1-P)

# Sensitivity studies

for (x in seq(3,n,1)) {
  y <- prob(p,x,n)
  s <- s + y
  print(s)
}

reliab <- function (N) {
  s <- 0
  if (N <= n) {
    for (x in seq(N,n,1)) {
    y <- prob(p,x,n)
    s <- s + y
  }
    return(s)}
  else {
    print("-")
    }
}

n <- 6
reliab(6)
1 - reliab(6)

P <- c()

for (n in seq(6,1,-1)) {
  for (u in seq(6,1,-1)) {
    P <- c(P,reliab(u))
  }
}

matrix(P, nrow = 6, ncol = 6)
diag(matrix(P, nrow = 6, ncol = 6))

