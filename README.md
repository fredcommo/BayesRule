```r
##############################
# Bayes Rule Demo: mixture with 2 groups
##############################
[Example]: https://github.com/fredcommo/BayesRule/blob/master/example.png

### [Example]

require(rGithubClient)
git <- getRepo('fredcommo/BayesRule')
sourceRepoFile(git, "bayesRule.R")

n1 <- 1e3; mu1 <- 0; s1 <- .25
n2 <- 3e3; mu2 <- 1; s2 <- .25
N <- n1+n2
x <- c(rnorm(n1, mu1, s1), rnorm(n2, mu2, s2))
trueClass <- rep(c("A", "B"), times=c(n1, n2))

model <- Mclust(x)
k <- as.factor(model$classification)
pars <- model$parameters
Mu <- pars$mean
V <- pars$variance$sigmasq
if(length(V)<length(Mu)) V <- rep(V, length(Mu))
P <- pars$pro

# Bayes' decision
C <- bayesRule(N, Mu, V, P)
C

# Visualization
hist(x, nclass=100, freq=FALSE)
dLines(N, Mu, V, P)
points(x, rep(0, length(x)), cex=.2, col = as.factor(trueClass))
abline(v = C, lwd=2, lty=2)
```
