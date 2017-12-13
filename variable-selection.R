source('./load.R')
## install.packages('glmnet')
## install.packages('bestglm')
library(leaps)
library(glmnet)
library(bestglm)
library(boot)
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
subset.simple <- function (data, method=NULL) {
    if (is.null(method)) {
        return(regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data))
    } else {
        return(regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method=method))
    }
}
#' Modified for logistic. These codes are from https://github.com/yufree/democode
#' Currently not use this function.
predict.regsubsets <- function(coef, newdata) {
    form  <-  as.formula(~.)
    mat  <-  model.matrix(form, newdata)
    xvars  <-  names(coef)
    res = mat[, xvars] %*% coef
    res = exp(res)/(1+exp(res))
    return(res)
}
clear.names <- function (names) {
    if (names[1] == "(Intercept)") {
        names = names[-1]
    }
    return(unique(gsub("[a-z]*", "", names[-1])))
}
gen.formula <- function (coef, y) {
    as.formula(paste(y, "~", paste(clear.names(names(coef)), collapse="+")))
}
error.glm <- function (formula, train.data, test.data, thresh) {
    glm.fit = glm(formula, data=train.data, family="binomial")
    glm.res = predict(glm.fit, data=test.data, type="response")
    if (any(glm.res < 0 | glm.res > 1)) stop ("로지스틱 회귀로 예측한 값이 0 또는 1이 아닙니다.")
    glm.pred = rep("NO", length(test.data[,1]))
    glm.pred[glm.res[glm.res>thresh]] = "YES"
    return(mean(glm.pred != test.data$BANKRUPTCY))
}
error.cv <- function (formula, data, thresh, seed.num) {
    data.len = length(data[,1])
    set.seed(seed.num)
    train = sample(1:data.len, ceiling(data.len/2))
    train.data = data[train,]
    test.data = data[-train,]

    return(error.glm(formula, train.data, test.data, 0.5))
}
error.loocv <- function (formula, data) {
}
cross.validation <- function (regfit, data, seed.num) {
    
    cp = summary(regfit)$cp
    bic = summary(regfit)$bic
    adjr2 = summary(regfit)$adjr2

    coef.cp = coef(regfit, which.min(cp))
    coef.bic = coef(regfit, which.min(bic))
    coef.adjr2 = coef(regfit, which.max(adjr2))

    ## vars.cp = clear.names(names(coef.cp))
    ## vars.bic = clear.names(names(coef.bic))
    ## vars.adjr2 = clear.names(names(coef.adjr2))

    ## formula.cp = gen.formula("BANKRUPTCY", vars.cp)
    ## formula.bic = gen.formula("BANKRUPTCY", vars.bic)
    ## formula.adjr2 = gen.formula("BANKRUPTCY", vars.adjr2)
    formula.cp = gen.formula(coef.cp, "BANKRUPTCY")
    formula.bic = gen.formula(coef.bic, "BANKRUPTCY")
    formula.adjr2 = gen.formula(coef.adjr2, "BANKRUPTCY")

    print(error.cv(formula.cp, data, 0.5, seed.num))
    ## set.seed(seed.num)
    ## train = sample(1:data.len, floor(data.len/2))
    ## train.data = data[train,]
    ## test.data = data[-train,]
    
    ## glmfit.cp = glm(formula.cp, data=train.data, family="binomial")
    ## glmfit.bic = glm(formula.bic, data=train.data, family="binomial")
    ## glmfit.adjr2 = glm(formula.adjr2, data=train.data, family="binomial")

    ## res.cp = predict(glmfit.cp, data=test.data, type="response")
    ## res.bic = predict(glmfit.bic, data=test.data, type="response")
    ## res.adjr2 = predict(glmfit.adjr2, data=test.data, type="response")
    ## if (any(res.cp < 0 | res.cp > 1)) stop ("예측한 Cp값이 0 또는 1이 아닙니다.")
    ## if (any(res.bic < 0 | res.bic > 1)) stop ("예측한 BIC값이 0 또는 1이 아닙니다.")
    ## if (any(res.adjr2 < 0 | res.adjr2 > 1)) stop ("예측한 Adjr2값이 0 또는 1이 아닙니다.")
}


regfit.full <- subset.simple(data)
regfit.fwd <- subset.simple(data, 'forward')
regfit.bwd <- subset.simple(data, 'backward')

plot.gen(regfit.full, './plots/tmp/full.png')
plot.gen(regfit.fwd, './plots/tmp/forward.png')
plot.gen(regfit.bwd, './plots/tmp/backward.png')

coef.print(regfit.full)
coef.print(regfit.fwd)
coef.print(regfit.bwd)

predict.regsubsets(coef(regfit.fwd, which.min(summary(regfit.fwd)$cp)), data)
