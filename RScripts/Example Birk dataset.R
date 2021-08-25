
  library(tidyverse)
  library(mgcv)
  
# read in the data for case 167
  
  dat <- read.csv("data/CaseID167.csv", sep = ";")
  glimpse(dat)

# Birk et al apply a Box Cox transformation to the data
# here we analyse it on the log scale  
  
# plot the raw data
  par(mfrow = c(2, 2))
  plot(y ~ x1, data = dat)
  plot(y ~ x2, data = dat)
  plot(x2 ~ x1, data = dat)

# log transform, scale to mean zero and sd one, and plot  
  y <- as.vector(scale(log(dat$y)))
  x1 <- as.vector(scale(log(dat$x1)))
  x2 <- as.vector(scale(log(dat$x2)))
  
  cor.test(x1, x2)

  par(mfrow = c(2, 2))
  plot(x1 ~ x2, main = paste("r =", round(cor(x1, x2), 2)))
  plot(y ~ x1, main = paste("r =", round(cor(y, x1), 2)))
  plot(y ~ x2, main = paste("r =", round(cor(y, x2), 2)))

# fit the models to the data and show the estimated parameters (Table 2)
  
  m1 <- gam(y ~ x1*x2, method = "ML")
  summary(m1)
  
  m2 <- gam(y ~ s(x1) + s(x2) + x1:x2, method = "ML")
  summary(m2)

  m3 <- gam(y ~ s(x1) + s(x2) + ti(x1, x2), method = "ML")
  summary(m3)

# plot
  par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))

# specify too.far (see vis.gam help)  
  tf <- 1
  
# upper and lower limits for z axis  
  z1 <- -2
  z2 <- 4

# plot figure
  vis.gam(m1, plot.type = "persp", color = "terrain", 
        theta = 140, phi = 25, too.far = tf, zlim = c(z1, z2), n.grid = 20,
        main = expression(paste(bold(a), "     y = ", beta[0], " + ", beta[1], x[1], " + ", beta[2], x[2],
                                " + ", beta[3], x[1], x[2])), zlab = "y")

  vis.gam(m2, plot.type = "persp", color = "terrain", 
        theta = 140, phi = 25, too.far = tf, zlim = c(z1, z2), n.grid = 20,
        main = expression(paste(bold(b), "     y = ", beta[0], " + s(", x[1], ") + s(", x[2],
                                ") + ", beta[3], x[1], x[2])), zlab = "y")

  vis.gam(m3, plot.type = "persp", color = "terrain", 
        theta = 140, phi = 25, too.far = tf, zlim = c(z1, z2), n.grid = 20,
        main = expression(paste(bold(c), "     y = ", beta[0], "+ s(", x[1], ") + s(", x[2],
                                ") + ti(", x[1], ",", x[2], ")")), zlab = "y")

# compare AIC values
  aic <- data.frame(aic = c(AIC(m1), AIC(m2), AIC(m3)),
                    model = 1:3)
  
  aic <- arrange(aic, aic) %>%
    mutate(delta = aic - min(aic))

  aic
  
