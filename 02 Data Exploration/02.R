library(ggplot2)

data.file <- file.path("data", "01_heights_weights_genders.csv")

## -------------------------------------------------------------------
## Numeric Summaries
## -------------------------------------------------------------------

heights.weights <- read.csv(data.file, header=TRUE, sep=",")
heights <- with(heights.weights, Height)
summary(heights)

## -------------------------------------------------------------------
## Means, Medians, and Modes
## -------------------------------------------------------------------

my.mean <- function(x) {
    return (sum(x) / length(x))
}

my.median <- function(x) {
    sorted.x <- sort(x)

    if (length(x) %% 2 == 0) {
        indices <- c(length(x) / 2, length(x) / 2 + 1)
        return(mean(sorted.x[indices]))
    } else {
        index <- ceiling(length(x) / 2)
        return(sorted.x[index])
    }
}

## test for my.mean and my.median
my.vector <- c(0, 100)
mean(my.vector)
median(my.vector)

my.vector <- c(0, 0, 100)
mean(my.vector)
median(my.vector)

my.mean(heights)
my.median(heights)
mean(heights) - my.mean(heights)
median(heights) - my.median(heights)

## -------------------------------------------------------------------
## Quantiles
## -------------------------------------------------------------------

c(min(heights), max(heights))
range(heights)

quantile(heights)
quantile(heights, probs=seq(0, 1, by=0.2))

## -------------------------------------------------------------------
## Standard Deviations and Variances
## -------------------------------------------------------------------

## range contains 50% data
c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))

## range contains 95% data
c(quantile(heights, probs=0.025), quantile(heights, probs=0.975))

## version 1
my.var <- function(x) {
    m <- mean(x)
    return(sum((x - m) ^ 2) / length(x))
}

## test my.var
my.var(heights) - var(heights)

## version 2
my.var <- function(x) {
    m <- mean(x)
    return(sum((x - m) ^ 2) / (length(x) - 1))
}

## test my.var
my.var(heights) - var(heights)

## range of one unit of variance is much larger
c(mean(heights) - var(heights), mean(heights) + var(heights))
range(heights)

my.sd <- function(x) {
    return(sqrt(my.var(x)))
}

## test my.sd
my.sd(heights) - sd(heights)

## one unit of std
c(mean(heights) - sd(heights), mean(heights) + sd(heights))
range(heights)

## roughly 50% data is within one std away from the mean
c(mean(heights) - sd(heights), mean(heights) + sd(heights))
c(quantile(heights, probs=0.25), quantile(heights, probs=0.75))

## -------------------------------------------------------------------
## Exploratory Data Visualization
## -------------------------------------------------------------------

ggplot(heights.weights, aes(x=Height)) +
    geom_histogram(binwidth=1)

## oversmoothing
ggplot(heights.weights, aes(x=Height)) +
    geom_histogram(binwidth=5)

## undersmoothing
## Sit back and take some coffee, this takes some time.
ggplot(heights.weights, aes(x=Height)) +
    geom_histogram(binwidth=0.001)

## Kernel Density Estimate (KDE), i.e., density plot
ggplot(heights.weights, aes(x=Height)) +
    geom_density()

## density plot for different gender
ggplot(heights.weights, aes(x=Weight, fill=Gender)) +
    geom_density()

## separate density plot
ggplot(heights.weights, aes(x=Weight, fill=Gender)) +
    geom_density() +
        facet_grid(Gender ~ .)

## playing with bell shape curve (normal distribution)
m <- 0
s <- 1
ggplot(data.frame(X=rnorm(100000, m, s)), aes(x=X)) +
    geom_density()

## normal distribution vs cauchy distribution
set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)

range(normal.values)
range(cauchy.values)

ggplot(data.frame(X = normal.values), aes(x = X)) + geom_density()
ggplot(data.frame(X = cauchy.values), aes(x = X)) + geom_density()

## gamma distribution
gamma.values <- rgamma(100000, 1, 0.001)
ggplot(data.frame(X=gamma.values), aes(x=X)) + geom_density()

## -------------------------------------------------------------------
## Visualizing the Relationships Between Columns
## -------------------------------------------------------------------

## scatter plot of height and weight
ggplot(heights.weights, aes(x=Height, y=Weight)) +
  geom_point()

## scatter plot with pattern
ggplot(heights.weights, aes(x=Height, y=Weight)) +
  geom_point() +
  geom_smooth()

## experiment with different data size
ggplot(heights.weights[1:20,], aes(x=Height, y=Weight)) +
  geom_point() +
  geom_smooth()

ggplot(heights.weights[1:200,], aes(x=Height, y=Weight)) +
  geom_point() +
  geom_smooth()

ggplot(heights.weights[1:2000,], aes(x=Height, y=Weight)) +
  geom_point() +
  geom_smooth()

## scatter plot with different gender
ggplot(heights.weights, aes(x=Height, y=Weight, color=Gender)) +
  geom_point()

## simple classification
heights.weights <- transform(heights.weights,
                             Male=ifelse(Gender=="Male", 1, 0))
logit.model <- glm(Male~Height + Weight, data=heights.weights,
                   family=binomial(link="logit"))
ggplot(heights.weights, aes(x=Weight, y=Height, color=Gender)) +
  geom_point() +
  stat_abline(intercept=-coef(logit.model)[1] / coef(logit.model)[2],
              slop=-coef(logit.model)[3] / coef(logit.model)[2],
              geom="abline", color="black")
