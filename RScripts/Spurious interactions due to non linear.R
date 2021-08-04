
  library(tidyverse)
  library(mgcv)
  library(parallel)

  
# example modified from 
# On the ambiguity of interaction and nonlinear main effects in a regime of dependent covariates
# Matuschek & Kliegl 2018 

  set.seed(123)
  n <- 2000
  x1 <- rnorm(n, sd = 0.5)
  u1 <- rnorm(n, sd = 0.5)

# z variables are correlated with x  
  x2 <- 0.5*x1 + u1
  
  cor(x1, x2)

# linear relationship  
  y1 <- rnorm(n, mean = x1 + 0.5*x2)
  
# x has non linear relationship
  y2 <- rnorm(n, mean = x1 + 0.5*x2 + 0.5*x1^2)
  
# interaction
  y3 <- rnorm(n, mean = x1 + 0.5*x2 + 0.5*x1*x2)

# fit linear interaction model
  m1 <- lm(y1 ~ x1*x2)
  m2 <- lm(y2 ~ x1*x2)
  m3 <- lm(y3 ~ x1*x2)

  
# plot function
  pf <- function(a, b, yl, xl, n) {
    plot(a ~ b, main = paste("r = ", round(cor(a, b), 2)), cex.lab = 1.5, 
         ylim = c(-4, 4), xlim = c(-1.8, 1.8), bty = "l",
         ylab = yl, xlab = xl, pch = 19, col = rgb(0, 0, 0, 0.1))
    mtext(n, line = 1, adj = 0, cex = 1.5)
  }

  par(mfrow = c(3, 2), mar = c(5, 5, 5, 2))
  
  ly1 <- expression(y[1])
  ly2 <- expression(y[2])
  ly3 <- expression(y[3])
  lx1 <- expression(x[1])
  lx2 <- expression(x[2])
  
  pf(y1, x1, ly1, lx1, "a"); pf(y1, x2, ly1, lx2, "b")
  pf(y2, x1, ly2, lx1, "c"); pf(y2, x2, ly2, lx2, "d")
  pf(y3, x1, ly3, lx1, "e"); pf(y3, x2, ly3, lx2, "f")

#########################################################################  
# residual plots for the three models
# function to plot
  pr <- function(m, title) {
    plot(m$residuals ~ predict(m), pch = 19, col = rgb(0, 0, 0, 0.2),
         xlab = "Fitted values", ylab = "Residuals", bty = "l", main = title, 
         cex.main = 1.5, cex.lab = 1.3)
    abline(h = 0)
  }
  
  
  par(mfrow = c(2, 2))
  
  pr(m1, expression(y[1]))
    mtext("a", line = 1, adj = 0, cex = 1.5)
  pr(m2, expression(y[2]))
    mtext("b", line = 1, adj = 0, cex = 1.5)
  pr(m3, expression(y[3]))
    mtext("c", line = 1, adj = 0, cex = 1.5)
  

######################################################## 
# now allow for nonlinear relationships
  
  m4 <- gam(y1 ~ s(x1) + s(x2) + x1:x2)  
  m5 <- gam(y2 ~ s(x1) + s(x2) + x1:x2)  
  m6 <- gam(y3 ~ s(x1) + s(x2) + x1:x2)  
  
######################################################## 
# construct a table of results
  
  tab <- data.frame(gen.model = rep(c("Linear", "Nonlinear", "Interaction"), each = 3),
                    param = rep(c("b1", "b2", "b3"), 3))
  
  tab$int.est <- round(c(summary(m1)$coef[2:4, 1],
                         summary(m2)$coef[2:4, 1],
                         summary(m3)$coef[2:4, 1]), 3)
  
  tab$int.se <-  round(c(summary(m1)$coef[2:4, 2],
                         summary(m2)$coef[2:4, 2],
                         summary(m3)$coef[2:4, 2]), 3)
  
  tab$int.p <-   round(c(summary(m1)$coef[2:4, 4],
                         summary(m2)$coef[2:4, 4],
                         summary(m3)$coef[2:4, 4]), 3)
  
  tab$nlin.est <- round(c(NA, NA, summary(m4)$p.coef[2], 
                          NA, NA, summary(m5)$p.coef[2], 
                          NA, NA, summary(m6)$p.coef[2]), 3)
  
  tab$nlin.est <- round(c(NA, NA, summary(m4)$p.coef[2], 
                          NA, NA, summary(m5)$p.coef[2], 
                          NA, NA, summary(m6)$p.coef[2]), 3)
  
  tab$nlin.se <-  round(c(NA, NA, summary(m4)$se[2], 
                          NA, NA, summary(m5)$se[2], 
                          NA, NA, summary(m6)$se[2]), 3)
  
  tab$nlin.p <-   round(c(NA, NA, summary(m4)$p.pv[2], 
                          NA, NA, summary(m5)$p.pv[2], 
                          NA, NA, summary(m6)$p.pv[2]), 3)
  
  tab
  

###############################################################################  
# function to alter correlation
  
  ac <- function(n) {
    x1 <- rnorm(n, sd = 0.5)
    u1 <- rnorm(n, sd = 0.5)
    
    b <- runif(1, -1, 1)
    
    x2 <- b*x1 + u1
    
    y1 <- rnorm(n, mean = x1 + 0.5*x2 + 0.5*x1^2)
    m1 <- lm(y1 ~ x1*x2)
    m2 <- gam(y1 ~ s(x1) + s(x2) + x1:x2)
    
    y2 <- rnorm(n, mean = x1 + 0.5*x2 + 0.5*x1*x2)
    m3 <- lm(y2 ~ x1*x2)
    m4 <- gam(y2 ~ s(x1) + s(x2) + x1:x2)
    
    
    return(c(cor(x1, x2), summary(m1)$coef[4, c(1, 4)], 
             summary(m2)$p.coef[2], summary(m2)$p.pv[2],
             summary(m3)$coef[4, c(1, 4)],
             summary(m4)$p.coef[2], summary(m4)$p.pv[2]))
  }
  

  set.seed(123)
# number of observations
  n <- 2000

# run simulations in parallel to speed up  
  num_cores <- detectCores() - 1
  my_cluster <- makeCluster(num_cores)
  
# number of times to simulate data  
  nrun <- 1000
  clusterExport(my_cluster, c("ac", "gam"))

  out <- parLapply(my_cluster, rep(n, nrun), function(x) ac(x))
  stopCluster(my_cluster)  
  
  out.all <- matrix(nrow = nrun, ncol = 9)
  for(i in 1:9) {
    out.all[, i] <- unlist(lapply(out, function(x) x[[i]]))
  }
  
  colnames(out.all) <- c("cor", "m1.est", "m1.p", "m2.est", "m2.p", "m3.est", "m3.p", "m4.est", "m4.p")
  out.all <- as.data.frame(out.all)

  cl1 <- ifelse(out.all$m1.p <= 0.05, "tomato", "skyblue")
  cl2 <- ifelse(out.all$m2.p <= 0.05, "tomato", "skyblue")
  cl3 <- ifelse(out.all$m3.p <= 0.05, "tomato", "skyblue")
  cl4 <- ifelse(out.all$m4.p <= 0.05, "tomato", "skyblue")
  
  par(mfrow = c(2, 2), oma = c(0, 0, 4, 4))
  
  plot(out.all$m1.est ~ out.all$cor, pch = 19, col = cl1, 
       ylab = expression(paste("Estimated interaction term  ", beta[3])), 
       xlab = "", bty = "l", cex.lab = 1.2, ylim = c(-0.5, 0.5))
  mtext("a", line = 1, adj = 0, cex = 1.3)
  abline(h = 0, lwd = 2)
  mtext("Fitted model", line = 5, cex = 1.2)
  mtext(expression(paste("y = ",beta[0]," + ",beta[1],x[1]," + ",beta[2],x[2]," + ",beta[3],x[1],x[2])), line = 3, cex = 1.2)

  plot(out.all$m2.est ~ out.all$cor, pch = 19, col = cl2, ylab = "", 
       xlab = "", bty = "l", cex.lab = 1.2, ylim = c(-0.5, 0.5))
  mtext("b", line = 1, adj = 0, cex = 1.3)
  abline(h = 0, lwd = 2)
  mtext("Fitted model", line = 5, cex = 1.2)
  mtext(expression(paste("y = ",beta[0], " + s(",x[1],") + s(",x[2],") + ",beta[3],x[1],x[2])), line = 3, cex = 1.2)
  text(1.2, 0, "Generating model", xpd = NA, srt = -90, cex = 1.4)
  text(1, 0, expression(paste("y = ",x[1]," + 0.5",x[2]," + 0.5",x[1],x[1])), xpd = NA, srt = -90, cex = 1.4)

  plot(out.all$m3.est ~ out.all$cor, pch = 19, col = cl3, 
       ylab = expression(paste("Estimated interaction term  ", beta[3])), 
       xlab = "Correlation between x1 and x2", bty = "l", cex.lab = 1.2)
  mtext("c", line = 1, adj = 0, cex = 1.3)
  abline(h = 0.5, lwd = 2)

  plot(out.all$m4.est ~ out.all$cor, pch = 19, col = cl4, ylab = "", 
       xlab = "Correlation between x1 and x2", bty = "l", cex.lab = 1.2)
  mtext("d", line = 1, adj = 0, cex = 1.3)
  abline(h = 0.5, lwd = 2)
  text(1.2, 0.5, "Generating model", xpd = NA, srt = -90, cex = 1.4)
  text(1, 0.5, expression(paste("y = ",x[1]," + 0.5",x[2]," + 0.5",x[1],x[2])), xpd = NA, srt = -90, cex = 1.4)

# number of false positives
  table(cl1)
  table(cl2)
  
  