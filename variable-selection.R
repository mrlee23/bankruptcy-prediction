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
    glm.pred = rep("NO", length(glm.res))
    glm.pred[glm.res>thresh] = "YES"
    return(mean(glm.pred != test.data$BANKRUPTCY))
}
error.cv <- function (formula, data, thresh, seed.num) {
    data.len = length(data[,1])
    set.seed(seed.num)
    train = sample(1:data.len, data.len/2)
    train.data = data[train,]
    test.data = data[-train,]

    return(error.glm(formula, train.data, test.data, thresh))
}
error.loocv <- function (formula, data, k=100, thresh, seed.num) {
    data.len = length(data[,1])
    CV = c()
    set.seed(seed.num)
    base = sample(1:data.len)
    count = floor(data.len/k)
    start = 1
    while (start < data.len) {
        end = start + count
        if (end > data.len) end = data.len
        test = base[c(start:end)]
        test.data = data[test,]
        train.data = data[-test,]
        CV = c(CV, error.glm(formula, train.data, test.data, thresh))
        start = end + 1
    }
    return(mean(CV))
}
cross.validation <- function (regfit, data, thresh, seed.num=11) {
    LOOCV_K = 200
    
    cp = summary(regfit)$cp
    bic = summary(regfit)$bic
    adjr2 = summary(regfit)$adjr2

    coef.cp = coef(regfit, which.min(cp))
    coef.bic = coef(regfit, which.min(bic))
    coef.adjr2 = coef(regfit, which.max(adjr2))

    formula.cp = gen.formula(coef.cp, "BANKRUPTCY")
    formula.bic = gen.formula(coef.bic, "BANKRUPTCY")
    formula.adjr2 = gen.formula(coef.adjr2, "BANKRUPTCY")

    print(formula.cp)
    print(formula.bic)
    print(formula.adjr2)
    
    result.cp = c(error.cv(formula.cp, data, thresh, seed.num),
                  error.loocv(formula.cp, data, LOOCV_K, thresh, seed.num),
                  error.loocv(formula.cp, data, 10, thresh, seed.num))
    
    result.bic = c(error.cv(formula.bic, data, thresh, seed.num),
                   error.loocv(formula.bic, data, LOOCV_K, thresh, seed.num),
                   error.loocv(formula.bic, data, 10, thresh, seed.num))
    
    result.adjr2 = c(error.cv(formula.adjr2, data, thresh, seed.num),
                     error.loocv(formula.adjr2, data, LOOCV_K, thresh, seed.num),
                     error.loocv(formula.adjr2, data, 10, thresh, seed.num))
    result = data.frame(cp=result.cp, bic=result.bic, adjr2=result.adjr2)
    rownames(result) <- c("cv", "loocv", "10-fold")
    return(result)
}

regfit.full <- subset.simple(data)
regfit.full <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data)
regfit.fwd <- subset.simple(data, 'forward')
regfit.fwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data)
regfit.bwd <- subset.simple(data, 'backward')
regfit.bwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data)

plot.gen(regfit.full, './plots/full.png')
plot.gen(regfit.fwd, './plots/forward.png')
plot.gen(regfit.bwd, './plots/backward.png')

coef.print(regfit.full)
coef.print(regfit.fwd)
coef.print(regfit.bwd)

result.full = cross.validation(regfit.full, data, 0.5, 15)
result.fwd = cross.validation(regfit.fwd, data, 0.5, 15)
result.bwd = cross.validation(regfit.bwd, data, 0.5, 15)


plot(x=c("CV", "LOOCV", "10-fold"),result[["cp"]], type="l")

png('./plots/variable-selection.png')
as.matrix(result)
matplot(as.matrix(result), type = c("b"),pch=1,col = 2:4)
legend("topright", legend = c("C_p", "BIC", "AdjR^2"), col=2:4, pch=1)
dev.off()
