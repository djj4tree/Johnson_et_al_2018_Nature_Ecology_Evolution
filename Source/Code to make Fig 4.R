# code for making Figure 4

library(RColorBrewer)

# read in trait data

wsg <- read.table("./Data/Wood density Fig 4a.txt", sep = "\t", header = TRUE)
lma <- read.table("./Data/LMA Fig 4b.txt", sep = "\t", header = TRUE)
sm <- read.table("./Data/Seed mass Fig 4c.txt", sep = "\t", header = TRUE)

# order mode levels
levels(wsg$mode)<-c('Understory', 'Transient', 'Canopy', 'Large Canopy')
levels(lma$mode)<-c('Understory', 'Transient', 'Canopy', 'Large Canopy')
levels(sm$mode)<-c('Understory', 'Transient', 'Canopy', 'Large Canopy')

# choose palette
cols <- brewer.pal(4, "Dark2")

# Make figure

par(mfrow = c(3, 1))

par(bty="L", mar=c(1,5,0.2,0))
boxplot(log(wsg$wsg)~wsg$mode, xlab="",xaxt='n', ann=FALSE, ylab="ln(Wood specific gravity)", col=cols, cex.axis=1.5, cex.lab=1.5)
title(main="a", font=2, adj=0.05, line=-1)
text("a",x=1.1, y=-0.2, cex=1)
text("b",x=2.1, y=-0.2, cex=1)
text("a",x=3.1, y=-0.2, cex=1)
text("c",x=4.1, y=-0.2, cex=1)

boxplot(lma$lma ~ lma$mode, xlab="", xlab="",xaxt='n', ann=FALSE, ylab = expression("LMA" ~
                                                                                        (g ~ m^{-2} ~ "")), col=cols, cex.axis=1.5, cex.lab=1.5)
title(main="b", font=2, adj=0.05, line=-1)

text("a", x=1.2, y=100, cex=1)
text("b", x=2.2, y=100, cex=1)
text("a", x=3.2, y=100, cex=1)
text("b", x=4.2, y=100, cex=1)

par(bty="L", mar=c(3,5,0.2,0))
boxplot(log(sm$seedmass)~sm$mode, xlab="", ylab="ln(Seed mass (g))", col=cols, cex.axis=1.5, cex.lab=1.5)
title(main="c", font=2, adj=0.05, line=-1)


