load("C:/Users/rebec/OneDrive/Documents/Grad School/MIS581/ICPSR_37392/DS0001/37392-0001-Data.rda")

load("C:/Users/rebec/OneDrive/Documents/ICPSR_37057/DS0001/37057-0001-Data.rda")

jail_census <- data.frame(da37392.0001)

View(jail_census)

mean(jail_census$WHITE)

mean(jail_census$ADP)

arrests_data <- data.frame(da37057.0001)
View(arrests_data)

View(arrests_data$OFFENSE)
mean(arrests_data$AW, na.rm=TRUE)
mean(arrests_data$AB, na.rm=TRUE)

library(ggplot2)
white <- jail_census$WHITE
black <- jail_census$BLACK
boxplot(jail_census$WHITE)
boxplot(white, black, horizontal = TRUE, names=c("White","Black"),main="White vs. Black Jail Populations")

t.test_knownvar <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, alternative = "two.sided") {
  M1 <- mean(x)
  M2 <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigma1 <- sqrt(V1)
  sigma2 <- sqrt(V2)
  S <- sqrt((V1 / n1) + (V2 / n2))
  statistic <- (M1 - M2 - m0) / S
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(statistic), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(statistic, lower.tail = TRUE)
  } else {
    pnorm(statistic, lower.tail = FALSE)
  }
  LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
  UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
  value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
  # print(sprintf("P-value = %g",p))
  # print(sprintf("Lower %.2f%% Confidence Limit = %g",
  #               alpha, LCL))
  # print(sprintf("Upper %.2f%% Confidence Limit = %g",
  #               alpha, UCL))
  return(value)
}

test <- t.test_knownvar(jail_census$WHITE, jail_census$BLACK,
                        V1 = 1, V2 = 1
)
test












