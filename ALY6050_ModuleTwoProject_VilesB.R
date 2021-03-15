N <- 10^4
set.seed(34)
##### PART 1-1 #####

## creating qtriangular
qtriangular<-function(p,a,b,c) {
  A <- a+sqrt((b-a)*(c-a)*p)
  B <- b-sqrt((b-a)*(b-c)*(1-p))
  q <- ifelse(p<(c-a)/(b-a), A, B)
  return(q)
}

## creating dam matrices
d1.b <- matrix(c(1.1,2.8,2,8,14.9,12,1.4,2,1.4,6.5,14.6,9.8,1.7,3.6,2.4,0,2.4,1.6), byrow=T, nrow=6)
d1.c <- matrix(c(13.2,19.1,14.2,3.5,7.4,4.9),byrow=T,nrow=2)

d2.b <- matrix(c(2.1,4.8,3,8.7,13.6,12.2,2.3,3,3,5.9,15,8.7,0,3.4,3.4,0,1.8,1.2),byrow=T,nrow=6)
d2.c <- matrix(c(12.8,20.1,15.8,3.8,8,5.7),byrow=T,nrow=2)

## simulation

## creating matrices and vectors to be filled by qtriangular
D1.B <- matrix(nrow=N,ncol=6)
D1.C <- matrix(nrow=N, ncol=2)

D2.B <- matrix(nrow=N,ncol=6)
D2.C <- matrix(nrow=N, ncol=2)

D1.B.Total <- vector()
D1.C.Total <- vector()
alpha1 <- vector()

D2.B.Total <- vector()
D2.C.Total <- vector()
alpha2 <- vector()

for(j in 1:N) {
  for(i in 1:6) {
    D1.B[,i] <- qtriangular(runif(1), d1.b[i,1], d1.b[i,2], d1.b[i,3])
    D2.B[,i] <- qtriangular(runif(1), d2.b[i,1], d2.b[i,2], d2.b[i,3])
  }
  for(i in 1:2) {
    D1.C[,i] <- qtriangular(runif(1), d1.c[i,1], d1.c[i,2], d1.c[i,3])
    D2.C[,i] <- qtriangular(runif(1), d2.c[i,1], d2.c[i,2], d2.c[i,3])
  }
  D1.B.Total[j] <- sum(D1.B[j,])
  D2.B.Total[j] <- sum(D2.B[j,])
  D1.C.Total[j] <- sum(D1.C[j,])
  D2.C.Total[j] <- sum(D2.C[j,])
  alpha1[j] <- D1.B.Total[j]/D1.C.Total[j]
  alpha2[j] <- D2.B.Total[j]/D2.C.Total[j]
}

##### PART 1-2 #####

## ratio histograms
## creating breaks
d1.min <- min(alpha1)-0.05
d1.max <- max(alpha1)+0.05
d1.range <- d1.max-d1.min
d1.breaks <- seq(d1.min,d1.max,by=d1.range/round(sqrt(N),0))

d2.min <- min(alpha2)-0.05
d2.max <- max(alpha2)+0.05
d2.range <- d2.max-d2.min
d2.breaks <- seq(d2.min,d2.max,by=d2.range/round(sqrt(N),0))

## histograms
hist(alpha1, freq=F, breaks = d1.breaks, main="Alpha 1 histogram", xlab="Alpha 1")
lines(density(alpha1), col="dark green",lwd=2)

hist(alpha2,freq=F,breaks=d2.breaks, main="Alpha 2 histogram", xlab="Alpha 2")
lines(density(alpha2), col="dark green",lwd=2)


## tables
breaks <- seq(0.9,2.1, by=0.05)
d1.cut <- cut(alpha1, breaks=breaks)
d1.groups <- transform(table(d1.cut))
d1.groups$Freq.Prob <- (d1.groups$Freq/sum(d1.groups$Freq))
d1.groups$Cum.Prob <- cumsum(d1.groups$Freq.Prob)
d1.groups

d2.cut <- cut(alpha2, breaks=breaks)
d2.groups <- transform(table(d2.cut))
d2.groups$Freq.Prob <- (d2.groups$Freq/sum(d2.groups$Freq))
d2.groups$Cum.Prob <- cumsum(d2.groups$Freq.Prob)
d2.groups

##### PART 1-3 #####
d1.b.mean <- vector()
d1.b.var <- vector()
d1.c.mean <- vector()
d1.c.var <- vector()

d2.b.mean <- vector()
d2.b.var <- vector()
d2.c.mean <- vector()
d2.c.var <- vector()

## creating functions
func.mean <- function(a) {
  x <- ((sum(a))/3)
  return(x)
}
func.varcalc <-function(a) {
  x <- (((a[1]^2)+(a[2]^2)+(a[3]^2)-(a[1]*a[2])-(a[1]*a[3])-(a[2]*a[3]))/18)
  return(x)
}

## calculating means and variances
for(i in 1:6) {
  d1.b.mean[i] <- func.mean(d1.b[i,])
  d1.b.var[i] <- func.varcalc(d1.b[i,])
  d2.b.mean[i] <- func.mean(d2.b[i,])
  d2.b.var[i] <- func.varcalc(d2.b[i,])
}
for(i in 1:2) {
  d1.c.mean[i] <- func.mean(d1.c[i,])
  d1.c.var[i] <- func.varcalc(d1.c[i,])
  d2.c.mean[i] <- func.mean(d2.c[i,])
  d2.c.var[i] <- func.varcalc(d2.c[i,])
}

## d1 table values

d1.e.total.mean.benefit <- sum(d1.b.mean)
d1.e.total.mean.cost <- sum(d1.c.mean)
d1.e.total.sd.benefit <- sqrt(sum(d1.b.var))
d1.e.total.sd.cost <- sqrt(sum(d1.c.var))
d1.e.total.ratio <- d1.e.total.mean.benefit/d1.e.total.mean.cost

d1.o.total.mean.benefit <- mean(D1.B.Total)
d1.o.total.mean.cost <- mean(D1.C.Total)
d1.o.total.sd.benefit <- sd(D1.B.Total)
d1.o.total.sd.cost <- sd(D1.C.Total)
d1.o.total.ratio <- d1.o.total.mean.benefit/d1.o.total.mean.cost
d1.o.ratio.mean <- mean(alpha1)
d1.o.ratio.sd <- sd(alpha1)

## d2 table values

d2.e.total.mean.benefit <- sum(d2.b.mean)
d2.e.total.mean.cost <- sum(d2.c.mean)
d2.e.total.sd.benefit <- sqrt(sum(d2.b.var))
d2.e.total.sd.cost <- sqrt(sum(d2.c.var))
d2.e.total.ratio <- d2.e.total.mean.benefit/d2.e.total.mean.cost

d2.o.total.mean.benefit <- mean(D2.B.Total)
d2.o.total.mean.cost <- mean(D2.C.Total)
d2.o.total.sd.benefit <- sd(D2.B.Total)
d2.o.total.sd.cost <- sd(D2.C.Total)
d2.o.total.ratio <- d2.o.total.mean.benefit/d2.o.total.mean.cost
d2.o.ratio.mean <- mean(alpha2)
d2.o.ratio.sd <- sd(alpha2)

## table creation
row.names <- c("Mean of Total Benefit",
               "SD of Total Benefit", 
               "Mean of Total Cost", 
               "SD of Total Cost", 
               "Mean Benefit/Mean Cost", 
               "Mean of Benefit-Cost Ratio", 
               "SD of Benefit-cost Ratio")

d1.expected <- c(d1.e.total.mean.benefit,
                 d1.e.total.sd.benefit,
                 d1.e.total.mean.cost,
                 d1.e.total.sd.cost,
                 d1.e.total.ratio,
                 NA,
                 NA)

d1.observed <- c(d1.o.total.mean.benefit,
                 d1.o.total.sd.cost,
                 d1.o.total.mean.cost,
                 d1.o.total.sd.cost,
                 d1.o.total.ratio,
                 d1.o.ratio.mean,
                 d1.o.ratio.sd)

d2.expected <- c(d2.e.total.mean.benefit,
                 d2.e.total.sd.benefit,
                 d2.e.total.mean.cost,
                 d2.e.total.sd.cost,
                 d2.e.total.ratio,
                 NA,
                 NA)

d2.observed <- c(d2.o.total.mean.benefit,
                 d2.o.total.sd.cost,
                 d2.o.total.mean.cost,
                 d2.o.total.sd.cost,
                 d2.o.total.ratio,
                 d2.o.ratio.mean,
                 d2.o.ratio.sd)

d1.table <- cbind(row.names,d1.observed,d1.expected)
d1.table <- as.data.frame(d1.table)
d1.table

d2.table <- cbind(row.names,d2.observed,d2.expected)
d2.table <- as.data.frame(d2.table)
d2.table

##### PART 2-1 #####

## Testing the Gamma Distribution on alpha1
## shape and scale paramaters
g.shape <- mean(alpha1)^2/var(alpha1)
g.scale <- mean(alpha1)/var(alpha1)
g.cut <- cut(alpha1, breaks=d1.breaks)

rg <- rgamma(N,shape=g.shape,scale=1/g.scale)
rg.cut <- cut(rg, breaks=d1.breaks)
c2test <- chisq.test(g.cut,rg.cut)
c2test
## chi square test passed 
## [1] 0.9920694
## X-squared 7604.923 

## histogram for rg
rg.min <- min(rg)-0.05
rg.max <- max(rg)+0.05
rg.range <- rg.max-rg.min
rg.breaks <- seq(rg.min,rg.max,by=rg.range/round(sqrt(N),0))

hist(rg, breaks=rg.breaks,freq = F, main="Gamma Distribution", xlab="X")
lines(density(rg), col="blue",lwd=2)

##### PART 3-1 #####
library(e1071)

a1.table <- c(min(alpha1),
              max(alpha1),
              mean(alpha1),
              median(alpha1),
              var(alpha1),
              sd(alpha1),
              skewness(alpha1),
              (sum((alpha1 - 2) >  0)/10000),
              (sum((alpha1 - 1.8) >  0)/10000),
              (sum((alpha1 - 1.5) >  0)/10000),
              (sum((alpha1 - 1.2) >  0)/10000),
              (sum((alpha1 - 1) >  0)/10000)
)

a2.table <- c(min(alpha2),
              max(alpha2),
              mean(alpha2),
              median(alpha2),
              var(alpha2),
              sd(alpha2),
              skewness(alpha2),
              (sum((alpha2 - 2) >  0)/10000),
              (sum((alpha2 - 1.8) >  0)/10000),
              (sum((alpha2 - 1.5) >  0)/10000),
              (sum((alpha2 - 1.2) >  0)/10000),
              (sum((alpha2 - 1) >  0)/10000)
)

p3.row.names <- c("Minimum",
                  "Maximum",
                  "Mean",
                  "Median",
                  "Variance",
                  "Standard Deviation",
                  "Skewness",
                  "P(Alpha > 2)",
                  "P(Alpha > 1.8)",
                  "P(Alpha > 1.5)",
                  "P(Alpha > 1.2)",
                  "P(Alpha > 1)"
)

p3.table <- as.data.frame(cbind(p3.row.names,a1.table,a2.table))
p3.table

p.a1.greater <- (sum((alpha1-alpha2)>0))/N

############################
##### NEEDED IN REPORT #####
############################

### Dam 1 Simulated Alpha Graph ###
hist(alpha1, freq=F, breaks = d1.breaks, main="Alpha 1 histogram", xlab="Alpha 1")
lines(density(alpha1), col="dark green",lwd=2)

### Dam 2 Simulated Alpha Graph ###
hist(alpha2,freq=F,breaks=d2.breaks, main="Alpha 2 histogram", xlab="Alpha 2")
lines(density(alpha2), col="dark green",lwd=2)

### Dam 1 Simulated Alpha Tab ###
d1.groups

### Dam 2 Simulated Alpha Tab ###
d2.groups

### Dam 1 Table ###
d1.table

### Dam 2 Table ###
d2.table

### Simulated Gamma Dist Graph ###
hist(rg, breaks=rg.breaks,freq = F, main="Gamma Distribution", xlab="X")
lines(density(rg), col="blue",lwd=2)

### Chi square for Gamma on alpha 1 ###
c2test

### Alpha Comparison Table ###
p3.table

### Prob A1 > A2 ###
p.a1.greater

