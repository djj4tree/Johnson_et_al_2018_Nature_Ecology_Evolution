# Code to reproduce Figure 2 Johnson et al. 2018 Nature Ecology and Evolution

library(tmvtnorm)
library(RColorBrewer)

# read in list of mode parameters

mode.pars <- readRDS("./Data/Mode_parameters.RDS")

# assign objects for each element of mode.pars

# mode means

clust.param.means <- mode.pars[[1]]

# mode covariances

clust.param.cov1 <- mode.pars[[2]]

# mean threshold for each mode

thr <- mode.pars[[3]]

# p1 parameter for each cluster to define upper limit

pone <- mode.pars[[4]]

# define maximum and minimum sizes to plot
max.sz=1700
min.sz=10

#############
# functions #
#############

pred.s <- function(pars, dbh = seq(min.size, max.sz), thresh) {
  pars <- as.numeric(pars)
  K    <- pars[1]
  ip1  <- pars[2]
  r1   <- pars[3]
  ip2  <- pars[4]
  r2   <- pars[5]
  dbh1 <- dbh[dbh < thresh]
  dbh2 <- dbh[dbh >= thresh]
  y1   <- K / (1 + exp(-r1 * (dbh1 - ip1)))
  y2   <- K / (1 + exp(-r2 * (dbh2 - ip2)))
  return(c(y1, y2))
}

make.poly.vecs <- function(surv.par.samp, dbh = seq(min.size, max.sz), thresh,
                           CI = c(0.05, 0.5, 0.95)) {
  surv.mat <- apply(surv.par.samp, MAR = 1, pred.s, dbh = dbh, thresh = thresh)
  CI.surv <- apply(surv.mat, MARGIN = 1, FUN =  quantile, probs = CI)
  return(CI.surv)
}

#### Make Figure

cols <- brewer.pal(length(clust.param.means), "Dark2")
cols.90 <- paste(cols, "10", sep = "") # CI for 90% quantiles (change this to 95?)
cols.50 <- paste(cols, "50", sep = "") # this is the 25, 75 quantiles

g.cols <- brewer.pal(length(clust.param.means) * 2, "Paired")
g.cols.90 <- paste(g.cols, "30", sep = "") # this makes the quantile polygons transparent
g.cols.50 <- paste(g.cols, "50", sep = "") # so that we see distinctions but use the same color.

samp.no <- 1000  # number of replicate draws for uncertainty analysis.. the more the smoother the curve.
dbh.vec <- seq(1, max.sz)

n.clus <- length(clust.param.means)
clust.pars <- vector("list", length = n.clus)
for(i in 1:n.clus) {
  thresh <- thr[i]
  surv.par.samp <- as.data.frame(rtmvnorm2(samp.no, mean = clust.param.means[[i]][1:5],
                                           sigma = clust.param.cov1[[i]], lower = c(0, -Inf, 0, -Inf, -Inf),
                                           upper = c(1, (pone[i]+1), Inf, Inf, 0)))
  
  clust.pars[[i]]$surv.vecs <- make.poly.vecs(surv.par.samp, dbh = dbh.vec,
                                              thresh = thresh, CI = c(0.05, 0.25, 0.5, 0.75, 0.95))
}


lab.cols <- c(rep("white", n.clus - 1), "black")
lab.cols2 <- c("white", "white", "black", "white")
clus.name <- c("a", "b", "c", "d")

# loop through from biggest cluster to smallest to make sure the frame is the right size
par(mfrow = c(n.clus, 1), mar = c(3, 5, 2, 3))
for(i in 1:n.clus){
  
  means <- as.list(clust.param.means[[i]])
  thresh <- thr[i]
  surv.vec <- clust.pars[[i]]$surv.vecs
  plot(dbh.vec[c(1, length(dbh.vec))], c(0, 1), type = "l", col = "white", ylab = "",
       xlab = "", axes = FALSE, log = "", ylim = c(0, 1))
  axis(1, cex.axis = 0.85)
  axis(2, cex.axis = 0.85)
  box()
  polygon(c(dbh.vec, rev(dbh.vec)), c(surv.vec[2, ], rev(surv.vec[4, ])),
          col = cols.50[i], border = NA)
  polygon(c(dbh.vec, rev(dbh.vec)), c(surv.vec[1, ], rev(surv.vec[5, ])),
          col = cols.90[i], border = NA)
  surv.out <- pred.s(unlist(means[1:5]), dbh.vec, thresh = thresh)
  lines(dbh.vec, surv.out, col = cols[i], lwd = 2)
  abline(v = thresh, lty = 3, col=cols[i])
  mtext(sprintf("%s", clus.name[i]), font= 2, cex = 0.75, adj = 0, line = 0.5)
  mtext("Probability of survival", 2, cex = 0.85, line = 2.5, col = lab.cols2[i],
        at = c(-10, 0.8), adj = 0)
  mtext("Diameter (mm)", 1, cex = 0.8, line = 2, col = lab.cols[i])
  
}
