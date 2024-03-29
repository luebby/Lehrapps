# Generate artificial data

set.seed(1896)
n <- 80

x <- rnorm(n, mean = 8, sd = 5)
y <- 8000 - 8*x + 0.5*(x-15)^2
y <- y + rnorm(n, sd=(24+8*(abs(x-10))))

energy <- data.frame(temperature = x, consumption = y)
write.csv(energy, "Leaderboard2/data/energy.csv", row.names = FALSE)

# Check:
plot(x,y)
erg <- lm(y~x, data = energy)
# Model
summary(erg)
# Score
mean(erg$residuals^2)

