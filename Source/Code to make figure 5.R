# Code for Figure 5

g <- read.table("./Data/growth by mode.txt", header=T, sep = "\t")
lvls<-c('Understory', 'Transient', 'Canopy', 'Emergent')
g$order<-as.factor(rep(c(1,2,3,4), each=14))

f <- read.table("./Data/Mode frequency.txt", header=T, sep = "\t")

widths<-f$freq

boxplot(g$growth~g$order, ylab="Annual growth rate (mm)",  ylim=c(0,5),width=widths)
text(x=1,y=1.5,labels ="a")
text(x=2,y=4.6,labels ="bc")
text(x=3,y=3.2,labels ="ab")
text(x=4,y=4.0,labels ="c")

