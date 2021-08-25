
# Figure 1

  pdf("data/Figure 1.pdf", width = 9, height = 7)

  par(mfcol = c(2, 2), mar = c(4, 4, 4, 4))

# plot where there is no interaction on the log scale
  
  b0 <- 0.1
  b1 <- 0.1
  b2 <- 0.1
  
  x1 <- seq(-10, 10, 0.1)
  
  x2 <- 1
  ly1 <- b0 + b1*x1 + b2*x2

  x2 <- 10
  ly2 <- b0 + b1*x1 + b2*x2
  
  y1 <- exp(ly1)
  y2 <- exp(ly2)
  
  plot(ly2 ~ x1, type = "l", bty = "l", lwd = 2, ylim = c(-1, 2.2),
       xlab = expression(x[1]), ylab = "log(y)", cex.lab = 1.3)
  lines(ly1 ~ x1, lwd = 2)
  
  mtext("a", line = 1, adj = 0, cex = 1.3)
  mtext(expression(paste("log(y) = ",beta[0]," + ",beta[1],x[1]," + ",beta[2],x[2])), line = 2, cex = 1.2)
  
  text(10, max(ly1 + 0.2), expression(paste(x[2], " = 1")), xpd = NA, cex = 1.2)
  text(10, max(ly2 + 0.2), expression(paste(x[2], " = 10")), xpd = NA, cex = 1.2)  
  
  
  plot(y2 ~ x1, type = "l", bty = "l", lwd = 2, ylim = c(0, 8),
       xlab = expression(x[1]), ylab = "y", cex.lab = 1.3)
    lines(y1 ~ x1, lwd = 2)

  mtext("c", line = 1, adj = 0, cex = 1.3)

  text(10, max(y1 + 0.5), expression(paste(x[2], " = 1")), xpd = NA, cex = 1.2)
  text(10, max(y2 + 0.5), expression(paste(x[2], " = 10")), xpd = NA, cex = 1.2)
    
# plot where there is no interaction on the untransformed scale  
  
  b0 <- 0.1
  b1 <- 0.1
  b2 <- 0.1
  
  x1 <- seq(1, 100, 0.1)
  
  x2 <- 1
  ly1 <- (log(b0 + b1*x1 + b2*x2))
  y1 <- exp(ly1)

  x2 <- 100
  ly2 <- (log(b0 + b1*x1 + b2*x2))
  y2 <- exp(ly2)
  
  plot(y2 ~ x1, type = "l", bty = "l", lwd = 2, ylim = c(0, 20),
       xlab = expression(x[1]), ylab = "y", cex.lab = 1.3)
  lines(y1 ~ x1, lwd = 2)
  
  mtext("b", line = 1, adj = 0, cex = 1.3)
  mtext(expression(paste("y = ",beta[0]," + ",beta[1],x[1]," + ",beta[2],x[2])), line = 2, cex = 1.2)
  
  text(100, max(y1 + 1), expression(paste(x[2], " = 1")), xpd = NA, cex = 1.2)
  text(100, max(y2 + 1), expression(paste(x[2], " = 100")), xpd = NA, cex = 1.2)

  plot(ly2 ~ x1, type = "l", bty = "l", lwd = 2, ylim = c(-2, 3),
       xlab = expression(x[1]), ylab = "log(y)", cex.lab = 1.3)
    lines(ly1 ~ x1, lwd = 2)
      
  mtext("d", line = 1, adj = 0, cex = 1.3)

  text(100, max(ly1 + 0.3), expression(paste(x[2], " = 1")), xpd = NA, cex = 1.2)
  text(100, max(ly2 + 0.3), expression(paste(x[2], " = 100")), xpd = NA, cex = 1.2)
  
  dev.off()
  
  
########################################################################
# Figure 2
  pdf("data/Figure 2.pdf", width = 9, height = 7)
  
  set.seed(123)
  nsim <- 10
  b0 <- 3
  b1 <- 0.1
  b2 <- 0.1
  
  x1 <- rep(c(10, 20), each = nsim)
  x2 <- rep(c(10, 20), nsim)

  table(paste(x1, x2))
    
  y <- rnorm(length(x1), exp(b0 + b1*x1 + b2*x2), 50)
  ly <- log(y)
  
  par(mfrow = c(2, 2))

# plot with x1 on x axis
  
  plot(ly ~ jitter(x1, 0.3), pch = 19, col = x2, cex = 1.8, xlim = c(4, 22),
       xaxt = "n", xlab = expression(x[1]), bty = "l", ylab = "log(y)", cex.lab = 1.3)
  axis(1, at = c(10, 20), labels = c("Low", "High"), cex = 1.3)
  lines(c(10, 20), c(mean(ly[x1 == 10 & x2 == 10]), mean(ly[x1 == 10 & x2 == 20])), col = x2[1], lwd = 2)
  lines(c(10, 20), c(mean(ly[x1 == 10 & x2 == 20]), mean(ly[x1 == 20 & x2 == 20])), col = x2[2], lwd = 2)
  mtext("a", line = 1, adj = 0, cex = 1.3)
  legend("topleft", 
         legend = c("Low", "High"), 
         col = c(x2[1], x2[2]), 
         pch = 19, 
         pt.cex = 2, 
         cex = 1.2, 
         text.col = "black",
         title = expression(x[2]))    

  plot(y ~ jitter(x1, 0.3), pch = 19, col = x2, cex = 1.8, xlim = c(4, 22),
       xaxt = "n", xlab = expression(x[1]), bty = "l", ylab = "y", cex.lab = 1.3)
  axis(1, at = c(10, 20), labels = c("Low", "High"), cex = 1.3)
  lines(c(10, 20), c(mean(y[x1 == 10 & x2 == 10]), mean(y[x1 == 10 & x2 == 20])), col = x2[1], lwd = 2)
  lines(c(10, 20), c(mean(y[x1 == 10 & x2 == 20]), mean(y[x1 == 20 & x2 == 20])), col = x2[2], lwd = 2)
  mtext("b", line = 1, adj = 0, cex = 1.3)
  
  dev.off()

# analyse as factor variables
  fx1 <- factor(x1)
  fx2 <- factor(x2)
  
  summary(lm(log(y) ~ fx1*fx2))
  summary(lm(y ~ fx1*fx2))
  summary(lm(y ~ fx1 + fx2))
  
  
