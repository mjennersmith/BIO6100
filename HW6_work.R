library(ggplot2) # for graphics
library(MASS) # for maximum likelihood estimation


z<-read.csv("C:/Users/msmit156/OneDrive - University of Vermont/Documents/test3/pad_sed_TP.csv", header=TRUE, na.strings= "NA")
str(z)
summary(z)
z<-z[-277,]

z <- na.exclude(z)



p1 <- ggplot(data=z, aes(x=Depth, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(p1)

p1 <-  p1 +  geom_density(linetype="dotted",size=0.75)
print(p1)









normPars <- fitdistr(z$Depth,"normal")
print(normPars)
str(normPars)
normPars$estimate["mean"] # note structure of getting a named attribute









meanML <- normPars$estimate["mean"]
sdML <- normPars$estimate["sd"]

xval <- seq(0,max(z$Depth),len=length(z$Depth))

stat <- stat_function(aes(x = xval, y = ..y..), fun = dnorm, colour="red", n = length(z$Depth), args = list(mean = meanML, sd = sdML)) 

p1 + stat








expoPars <- fitdistr(z$Depth,"exponential")
rateML <- expoPars$estimate["rate"]

stat2 <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(z$Depth), args = list(rate=rateML))
p1  + stat + stat2










stat3 <- stat_function(aes(x = xval, y = ..y..), fun = dunif, colour="darkgreen", n = length(z$Depth), args = list(min=min(z$Depth), max=max(z$Depth)))
p1 + stat + stat2 + stat3










gammaPars <- fitdistr(z$Depth,"gamma")
shapeML <- gammaPars$estimate["shape"]
rateML <- gammaPars$estimate["rate"]

stat4 <- stat_function(aes(x = xval, y = ..y..), fun = dgamma, colour="brown", n = length(z$Depth), args = list(shape=shapeML, rate=rateML))
p1 + stat + stat2 + stat3 + stat4








#Non simulated
pSpecial <- ggplot(data=z, aes(x=Depth/(max(Depth + 0.1)), y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) + 
  xlim(c(0,1)) +
  geom_density(size=0.75,linetype="dotted")

betaPars <- fitdistr(x=z$Depth/max(z$Depth + 0.1),start=list(shape1=1,shape2=2),"beta")
shape1ML <- betaPars$estimate["shape1"]
shape2ML <- betaPars$estimate["shape2"]

statSpecial <- stat_function(aes(x = xval, y = ..y..), fun = dbeta, colour="orchid", n = length(z$Depth), args = list(shape1=shape1ML,shape2=shape2ML))
pSpecial + statSpecial








# best fit - exponential 

expoPars <- fitdistr(z$Depth,"exponential")
rateML <- expoPars$estimate["rate"]

print(expoPars)
str(expoPars)
expoPars$estimate["rate"]

myExp <- hist(rexp(n=1054, rate=0.5200658))
  
ExpLine  <- stat_function(aes(x = xval, y = ..y..), fun = dexp, colour="blue", n = length(1054), args = list(rate=0.5200658))
myExp + ExpLine                  





num <- 1:1054
var <- rexp(n=1054, rate=0.5200658)
frame <- data.frame(num,var)

myExp <- ggplot(data=frame, aes(x=var, y=..density..)) +
  geom_histogram(color="grey60",fill="cornsilk",size=0.2) 
print(myExp)

ExpLine<- stat_function(aes(x = var, y = ..y..), fun = dexp, colour="blue", n = length(1054), args = list(rate=0.5200658))

print(ExpLine)

ExpLine + myExp



library(ggplot2)

num <- 1:1054
var <- rexp(n=1054, rate=0.5200658)
frame <- data.frame(num, var)

myExp <- ggplot(data=frame, aes(x=var)) +
  geom_histogram(aes(y=..density..), color="grey60", fill="cornsilk", binwidth = 0.2) + 
  stat_function(fun = dexp, args = list(rate=0.5200658), color="blue", size=1)

print(myExp)
