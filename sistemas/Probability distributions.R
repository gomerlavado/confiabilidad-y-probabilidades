library(dplyr)
library(tidyverse)

N <- 1000

time <- seq(0,18,1)
failures <- c(140,85,75,68,60,53,48,43,38,34,31,28,40,60,75,60,42,15,5)
cumfail <- append(cumsum(failures),0,after = 0)
cumfail[!cumfail == 1000]
survivors <- 1000 - cumfail[!cumfail == 1000]

df <- data.frame(Time = time, 
                 Failures = failures, 
                 CumFail = cumfail[!cumfail == 1000],
                 Survivors = survivors,
                 FailFunct = failures/1000,
                 CumFailDistr = cumfail[!cumfail == 1000]/1000,
                 SurvFunct = survivors/1000
                 )

hazrate <- df$Failures/((df$Survivors + c(df$Survivors[-1],0))/2)

df[,"HazRate"] <- hazrate
df

ggplot(data = df, aes(Time,HazRate)) +
  geom_point(color = "royalblue") +
  geom_line(color = "royalblue") + 
  theme_bw() +
  labs(title = "Hazard rate", 
       x = "Time interval in 100 hrs",
       y = paste("\u03bb","(t)"))

ggplot(data = df, aes(Time,SurvFunct)) +
  geom_point(color = "darkred") +
  geom_line(color = "darkred") + 
  theme_bw() +
  labs(title = "Survivor function",
       x = "Time interval in 100 hrs",
       y = "R(t)")

ggplot(data = df, aes(Time,FailFunct)) +
  geom_point(color = "darkgreen") +
  geom_line(color = "darkgreen") + 
  theme_bw() +
  labs(title = "Failure function density",
       x = "Time interval in 100 hrs",
       y = "f(t)")

ggplot(data = df, aes(Time,CumFailDistr)) +
  geom_point(color = "orange") +
  geom_line(color = "orange") + 
  theme_bw() +
  labs(title = "Cumulative failure distribution",
       x = "Time interval in 100 hrs",
       y = "Q(t)")

# Example 6.1

fyear100 <- 0.5/100
lambda <- fyear100*10

prob <- function(x,t){
  mu <- lambda*t
  P <- mu^x * exp(-mu) / factorial(x)
  return(P)
}

df <- data.frame(NumFaults = seq(0,10,1),
                 Prob20 = mapply(prob, seq(0,10,1), 20),
                 Prob40 = mapply(prob, seq(0,10,1), 40)
                 )

barplot(df$Prob20,
        xlab = "NumFaults", ylab = "Probability", 
        col = c("royalblue"))

barplot(df$Prob40,
        xlab = "NumFaults", ylab = "Probability", 
        col = c("darkred"))

ggplot(data = df) + 
  geom_bar(aes(x = NumFaults, y = Prob20), 
           stat = "identity", 
           fill = "lightblue",
           color = "lightblue") +
  geom_bar(aes(x = NumFaults, y = Prob40), 
           stat = "identity", 
           fill = "orange",
           color = "orange",
           alpha = 0.5 ) +
  theme_bw()

# Example 6.4
n <- 2000
mu <- 1000
sigma <- 200
q <- 700 # query

x <- seq(-4, 4, length = 100) * sigma + mu

f <- dnorm(x, mu, sigma)

u <- seq(min(x), q, length = 100)

plot(x, f, type = "l", lwd = 2, col = "orange",
     xlab = "Burning hours", ylab = "Probability")

abline(v = mu, lwd = 2, col = "black")

pnorm(700, mu, sigma) * n

polygon(c(min(x), u, q), c(0, dnorm(u, mu, sigma), 0),
        col = rgb(0.5, 0.5, 0.5, alpha = 0.5))

text(q, 0.001, "6.68%")

(pnorm(1300, mu, sigma) - pnorm(900, mu, sigma))* n

u <- seq(900, 1300, length = 100)

polygon(c(900, u, 1300), c(0, dnorm(u, mu, sigma), 0),
        col = rgb(0.5, 0.5, 0.5, alpha = 0.5))

text(1000, 0.001, "62.5%")

plot(x, pnorm(x, mu, sigma), type = "l", lwd = 2, col = "royalblue",
     xlab = "Burning hours", ylab = "Cumulative distribution")

# Example 6.7

lamp_life_data <- c(854, 1284, 1001, 911, 1168, 963, 1279, 1494, 798, 1599,
                    1357, 1090, 1082, 1494, 1684, 1281, 590, 960, 1310, 1571,
                    1355, 1502, 1251, 1666, 778, 1200, 849, 1454, 919, 1484,
                    1550, 628, 1325, 1073, 1273, 1710, 1734, 1928, 1416, 1465,
                    1608, 1367, 1152, 1393, 1339, 1026, 1299, 1242, 1508, 705,
                    1199, 1155, 822, 1448, 1623, 1084, 1220, 1650, 1091, 210,
                    1058, 1930, 1365, 1291, 683, 1399, 1198, 518, 1199, 2074,
                    811, 1137, 1185, 892, 937, 945, 1215, 905, 1810, 1265)

max <- max(lamp_life_data)
min <- min(lamp_life_data)
n <- length(lamp_life_data)

# Sturges' rule
k <- 1 + 3.3*log(n, 10)
print(k)
w <- (max- min)/k
print(w)

low_val <- 199.5
high_val <- 199.5+200*10
print(high_val)
w <- 200

x_breaks <- seq(low_val, high_val, w)
x_mid <- seq(low_val + w/2, high_val - w/2, w)

ClassFreq <- cut(lamp_life_data, breaks = x_breaks)
print(ClassFreq)

class <- table(ClassFreq)
df <- data.frame(class)
df$Midpoint <- x_mid
df$RelFreq <- df$Freq / length(lamp_life_data)

df$CumFreq <- cumsum(df$Freq)
df$CumRelFreq <- cumsum(df$RelFreq)

df
view(df)

ggplot(data = df,aes(x=ClassFreq)) + geom_bar()


