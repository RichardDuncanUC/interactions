
  library(tidyverse)
  library(car)
  library(mgcv)

  source("c:\\users\\s429217\\onedrive\\data\\bugs\\birk\\R-script\\functions.r")  
  
  met <- read.csv("c:\\users\\s429217\\onedrive\\data\\bugs\\birk\\data\\0_Metadata.csv", sep = ";")
  glimpse(met)
  
  names(met)[1:2] <- c("case", "study")
  
# drop experiments which can lack continuous variables
  met <- met %>%
    filter(Study_type != "Experiment")
  
  table(met$Study_type)
  
# list of case studies 
  cs <- met$case
  cs
  
# case study file names 
  fn <- ifelse(cs < 10, 
               paste0("c:\\users\\s429217\\onedrive\\data\\bugs\\birk\\data\\CaseID00", cs, ".csv"),
               paste0("c:\\users\\s429217\\onedrive\\data\\bugs\\birk\\data\\CaseID0", cs, ".csv"))

  fn <- ifelse(cs >= 100, 
               paste0("c:\\users\\s429217\\onedrive\\data\\bugs\\birk\\data\\CaseID", cs, ".csv"), fn)

#################################################################################
# read the data into a consistent format
# a function to do this
  
  readin <- function(x) {
    temp <- read.csv(x, sep = ";")
    names(temp)[1] <- "id"
    
    # is the last column all missing values?
    if(sum(is.na(temp[, ncol(temp)])) == nrow(temp)) { 
      temp <- temp[, -ncol(temp)]
    }

  # check again: is the last column all missing values?
    if(sum(is.na(temp[, ncol(temp)])) == nrow(temp)) { 
      temp <- temp[, -ncol(temp)]
    }
  
  # drop any remaining missing rows
  temp <- drop_na(temp)    
  
  # determine number of random effects
  nre <- 0
  if(ncol(temp) == 5) nre <- 1
  if(ncol(temp) == 6) nre <- 2
  
  if(nre == 0) {
    temp$RE1 <- NA
    temp$RE2 <- NA
  }
  
  if(nre == 1) {
    temp$RE2 <- NA
  }
  
  names(temp)[5:6] <- c("RE1", "RE2")
  temp$RE1 <- as.character(temp$RE1)
  temp$RE2 <- as.character(temp$RE2)
  temp$id <- as.character(temp$id)

  temp$case <- as.numeric(substr(x, 53,55))

  return(temp)
    
  }

##################################################################################  
# use the function to read the data in
  alldat <- readin(fn[1])
  
  for(i in 2:length(fn)) {
    alldat <- bind_rows(alldat, readin(fn[i]))
  }
  
  glimpse(alldat)

##################################################################################  
# exclude studies with too few observations 
# too few categories for y, x1 and x2
# and too many random effect categories for the number of data points
  dat <- alldat %>%
    group_by(case) %>%
    mutate(nsamp = n(),
           cat1 = length(table(x1)),
           cat2 = length(table(x2)),
           caty = length(table(y)),
           nre1 = length(table(RE1, exclude = NULL)),
           nre2 = length(table(RE2, exclude = NULL))) %>%
    filter(nsamp >= 100) %>%
    filter(caty > 10 & cat1 > 10 & cat2 > 10) %>%
    ungroup()
  
  glimpse(dat)

# number of random effect categories  
  table(dat$nre1)
  table(dat$nre2)
  
# if nre1 is too large remove that random effect and only use RE2
  dat$RE1 <- ifelse(dat$nre1 > dat$nsamp/2, dat$RE2, dat$RE1)
  dat$RE2 <- ifelse(dat$nre1 > dat$nsamp/2, NA, dat$RE2)

# number of random effect categories  
  table(dat$nre1)
  table(dat$nre2)
  
# number of random effect columns
  dat <- dat %>%
    group_by(case) %>%
    mutate(re1 = ifelse(sum(!is.na(RE1)) > 0, 1, 0),
           re2 = ifelse(sum(!is.na(RE2)) > 0, 1, 0)) 
  
# attach study numbers
  sn <- met %>%
    select(case, study) 
  
  dat <- left_join(dat, sn)

# numbers of studies with 0, 1 or 2 random effects
  re <- dat %>%
    select(case, re1, re2) %>%
    unique()
  
  table(re$re1, re$re2)
  
# number of cases per study
  ncs <- dat %>%
    select(case, study, nsamp) %>%
    unique()
  
  print.data.frame(ncs)
  
# number of cases per study
  table(ncs$study)
  
# new list of cases having filtered
  cs <- unique(dat$case)
  

#################################################################################  
###############################################################################
# log scale  
  
  sub.dat <- dat[dat$case == 167, ]

  y <- as.vector(scale(log(sub.dat$y)))
  x1 <- as.vector(scale(log(sub.dat$x1)))
  x2 <- as.vector(scale(log(sub.dat$x2)))
  
  cor.test(x1, x2)

  par(mfrow = c(2, 2))
  plot(x1 ~ x2, main = paste("r =", round(cor(x1, x2), 2)))
  plot(y ~ x1, main = paste("r =", round(cor(y, x1), 2)))
  plot(y ~ x2, main = paste("r =", round(cor(y, x2), 2)))

  m0 <- gam(y ~ x1 + x2, method = "ML")
  summary(m0)
    
  m1 <- gam(y ~ x1*x2, method = "ML")
  summary(m1)
  
  m2 <- gam(y ~ s(x1) + s(x2) + x1:x2, method = "ML")
  summary(m2)

  m3 <- gam(y ~ s(x1) + s(x2) + ti(x1, x2), method = "ML")
  summary(m3)

  par(mfrow = c(2, 2), mar = c(1, 1, 1, 1))
  
  tf <- 1
  z1 <- -2
  z2 <- 4

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


  aic <- data.frame(aic = c(AIC(m1), AIC(m2), AIC(m3)),
                    model = 1:3)
  
  aic <- arrange(aic, aic) %>%
    mutate(delta = aic - min(aic))
  
  aic
  
