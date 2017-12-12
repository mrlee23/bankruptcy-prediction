## install.packages('glmnet')
library(glmnet)
library(leaps)
source('./load.R')
plot.gen <- function (regfit, output.file) {
    regfit.summary = summary(regfit)
    png(output.file)
    par(mfrow=c(1,3))

    plot(regfit.summary$cp,xlab="Number of Variables",ylab="C_p",type="l")
    points(which.min(regfit.summary$cp),regfit.summary$cp[which.min(regfit.summary$cp)],col="blue",cex=2,pch=20)

    plot(regfit.summary$bic,xlab="Number of Variables",ylab="BIC",type="l")
    points(which.min(regfit.summary$bic),regfit.summary$bic[which.min(regfit.summary$bic)],col="blue",cex=2,pch=20)

    plot(regfit.summary$adjr2,xlab="Number of Variables",ylab="Adj R^2",type="l")
    points(which.max(regfit.summary$adjr2),regfit.summary$adjr2[which.max(regfit.summary$adjr2)],col="blue",cex=2,pch=20)

    dev.off()
}
coef.print <- function (regfit) {
    regfit.summary = summary(regfit)
    print("C_p")
    print(which.min(regfit.summary$cp))
    print(coef(regfit,which.min(regfit.summary$cp)))
    print("BIC")
    print(which.min(regfit.summary$bic))
    print(coef(regfit,which.min(regfit.summary$bic)))
    print("Ajr^2")
    print(which.max(regfit.summary$adjr2))
    print(coef(regfit,which.max(regfit.summary$adjr2)))
}
subset.simple <- function (data, method) {
     ## regfit.full <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data)
     ## regfit.fwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method="forward")
     ## regfit.bwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method="backward")
    ## return(c(regfit.full, regfit.fwd, regfit.bwd))
    if (method == 'full') {
            return(regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data))
        } else {
            return(regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method=method))
            }
}
?regsubsets
dim(data)[2]
regfit.full <- regsubsets(BANKRUPTCY ~ . , nvmax=31, data=data)
regfit.fwd <- regsubsets(BANKRUPTCY ~ . , data=data, method="forward")
regfit.bwd <- regsubsets(BANKRUPTCY ~ . , data=data, method="backward")
summary(regfit.full)
plot.gen(regfit.full, './plots/subset.png')

regfit.full <- subset.simple(data, 'full')
regfit.fwd <- subset.simple(data, 'forward')
regfix.bwd <- subset.simple(data, 'backward')
plot.gen(regfit.fwd, './plots/forward.png')
plot.gen(regfit.bwd, './plots/backward.png')
## print(summary(regfit.full))


coef.print(regfit.full)
coef.print(regfit.fwd)
coef.print(regfit.bwd)
