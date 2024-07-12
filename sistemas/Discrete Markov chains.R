library(markovchain)
library(dslabs)
library(tidyverse)
library(ggplot2)

system_states <- c("one", "two")
system_states

# stochastic transitional probability matrix
tr_matrix <- matrix(c(0.5, 0.5, 0.25, 0.75), nrow = 2, 
                byrow = TRUE, 
                dimnames = list(system_states, system_states))
tr_matrix

markov_chain <- new("markovchain", states = system_states,
                    byrow = TRUE,
                    transitionMatrix = tr_matrix,
                    name = "Markov Chain")
markov_chain
class(markov_chain)

markov_chain^2

steadyStates(markov_chain)

tr_behav <- function(x) {
  query <- markov_chain^x
  return (query[1,])
}

state_1 <- sapply(1:6, tr_behav)[1,]
state_2 <- sapply(1:6, tr_behav)[2,]

df <- data.frame(time_interval = 1:6, state_1 = state_1, state_2 = state_2)

library(MASS) 
library(reshape2) 
library(reshape) 

data <- melt(df, id = c("time_interval"))
data

ggplot(data = data, aes(x = time_interval, y = value, colour = variable)) +
  geom_line() + ylim(0,1)

# Example 8.1

system_states <- c("one", "two", "three")
system_states

tr_matrix <- matrix(c(0.75, 0.25, 0, 0, 0.5, 0.5, 1/3, 1/3, 1/3), nrow = 3, 
                    byrow = TRUE, 
                    dimnames = list(system_states, system_states))
tr_matrix

markov_chain <- new("markovchain", states = system_states,
                    byrow = TRUE,
                    transitionMatrix = tr_matrix,
                    name = "Markov Chain")
markov_chain

steadyStates(markov_chain)

cat("P1 = ", 4/11)
cat("P3 = ", 3/11)

truncated_matrix <- tr_matrix[-c(3), -c(3)]
truncated_matrix

library(matlib)
inv(diag(2) - truncated_matrix)

state_1 <- sapply(1:6, tr_behav)[1,]
state_2 <- sapply(1:6, tr_behav)[2,]
state_3 <- sapply(1:6, tr_behav)[3,]

df <- data.frame(time_interval = 1:6, state_1 = state_1, state_2 = state_2, state_3 = state_3)
df

library(MASS) 
library(reshape2) 
library(reshape) 

data <- melt(df, id = c("time_interval"))
data

ggplot(data = data, aes(x = time_interval, y = value, colour = variable)) +
  geom_line() + ylim(0,1)

plot(markov_chain^3)

