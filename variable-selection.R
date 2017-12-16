source('./load.R')
## install.packages('glmnet')
## install.packages('bestglm')
library(leaps)
library(glmnet)
library(bestglm)
library(boot)

#' Plot Gen
#'
#' Subset selection의 C_p, BIC, AdjR^2 로 plot을 그린다.
#'
#' @param regfit regsubsets 으로 적합시킨 객체
#' @param output.file plot을 저장할 파일
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

#' Coef Print
#'
#' Subset selection의 C_p, BIC, AdjR^2의 최적화된 계수를 프린트한다.
#'
#' @param regfit regsubsets 으로 적합시킨 객체
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

#' Clear Names
#'
#' 질적 Class에 따라 변수로 나눠진 변수를 하나의 변수로 만들어 반환
#'
#' @param names 변수 이름을 담고 있는 vector
#'
#' @return Unique한 변수 이름 vector
clear.names <- function (names) {
    if (names[1] == "(Intercept)") {
        names = names[-1]
    }
    return(unique(gsub("[a-z]*", "", names[-1])))
}

#' Gen Formula
#'
#' 변수를 이용해서 glm에서 사용할 Formula을 생성한다.
#'
#' @param vars 사용할 변수 vector
#' @param y 반응변수로 사용할 변수
#'
#' @return formula 반환
gen.formula <- function (vars, y) {
    as.formula(paste(y, "~", paste(clear.names(vars), collapse="+")))
}

#' Error Glm
#'
#' glm의 오분류율을 반환
#'
#' @param formula 사용할 formula
#' @param train.data 트레이닝할 데이터
#' @param test.data 테스트할 데이터
#' @param thresh Threshold 설정(로지스틱)
#'
#' @return 오분류율 반환
error.glm <- function (formula, train.data, test.data, thresh) {
    glm.fit = glm(formula, data=train.data, family="binomial")
    glm.res = predict(glm.fit, data=test.data, type="response")
    if (any(glm.res < 0 | glm.res > 1)) stop ("로지스틱 회귀로 예측한 값이 0 또는 1이 아닙니다.")
    glm.pred = rep("NO", length(glm.res))
    glm.pred[glm.res>thresh] = "YES"
    return(mean(glm.pred != test.data$BANKRUPTCY))
}

#' Error CV
#'
#' Validation set approach의 오분류율을 반환
#'
#' @param formula 사용할 formula
#' @param data 사용할 데이터
#' @param thresh 로지스틱 Threshold
#' @param seed.num 같은 샘플을 사용하기 위해서 set.seed에서 사용할 번호
#'
#' @return 오분류율 반환
error.cv <- function (formula, data, thresh, seed.num) {
    data.len = length(data[,1])
    set.seed(seed.num)
    train = sample(1:data.len, data.len/2)
    train.data = data[train,]
    test.data = data[-train,]

    return(error.glm(formula, train.data, test.data, thresh))
}

#' Error LOOCV
#'
#' Validation set approach의 오분류율을 반환
#'
#' @param formula 사용할 formula
#' @param data 사용할 데이터
#' @param k=100 LOOCV로 사용할 K값(데이터가 많을 경우 설정해놓는 것이 LOOCV와 비슷한 효과를 가지면서 Computation 시간을 줄일 수 있음. k-fold와 동일)
#' @param thresh 로지스틱 Threshold
#' @param seed.num 같은 샘플을 사용하기 위해서 set.seed에서 사용할 번호
#'
#' @return 오분류율 반환
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

#' Subset CV
#'
#' regsubsets로 적합시킨 객체를 이용해 Cross-validation(Validatin-set approach, LOOCV, 10-fold)를 구한다.
#'
#' @param regfit regsubsets로 적합시킨 객체
#' @param data 사용할 데이터
#' @param thresh Threshold
#' @param seed.num 샘플 시드넘버
#'
#' @return C_p, BIC, AdjR^2의 CV, LOOCV, 10-fold 값
subset.cv <- function (regfit, data, thresh, seed.num) {
    LOOCV_K = 200
    
    cp = summary(regfit)$cp
    bic = summary(regfit)$bic
    adjr2 = summary(regfit)$adjr2

    coef.cp = coef(regfit, which.min(cp))
    coef.bic = coef(regfit, which.min(bic))
    coef.adjr2 = coef(regfit, which.max(adjr2))

    formula.cp = gen.formula(names(coef.cp), "BANKRUPTCY")
    formula.bic = gen.formula(names(coef.bic), "BANKRUPTCY")
    formula.adjr2 = gen.formula(names(coef.adjr2), "BANKRUPTCY")

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

#' Cross Validation
#'
#' subset.CV를 일반화한 함수
#'
#' @param vars 사용할 변수(반응 변수는 BANKRUPTCY로 설정되어 있음(data dependency 있음))
#' @param data 사용할 데이터
#' @param thresh Threshold
#' @param seed.num 샘플 시드넘버
#'
#' @return CV, LOOCV, 10-fold 값
cross.validation <- function (vars, data, thresh, seed.num) {
    LOOCV_K = 200
    formula = gen.formula(vars, "BANKRUPTCY")
    result.cv = error.cv(formula, data, thresh, seed.num)
    result.loocv = error.loocv(formula, data, LOOCV_K, thresh, seed.num)
    result.10fold = error.loocv(formula, data, 10, thresh, seed.num)
    return(data.frame(cv=result.cv, loocv=result.loocv, "10-fold"=result.10fold))
}

regfit.full <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data)
regfit.fwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method="forward")
regfit.bwd <- regsubsets(BANKRUPTCY ~ ., nvmax=dim(data)[2], data=data, method="backward")

plot.gen(regfit.full, './plots/full.png')
plot.gen(regfit.fwd, './plots/forward.png')
plot.gen(regfit.bwd, './plots/backward.png')

coef.print(regfit.full)
coef.print(regfit.fwd)
coef.print(regfit.bwd)

result.full = subset.cv(regfit.full, data, 0.1, 15)
result.fwd = subset.cv(regfit.fwd, data, 0.1, 15)
result.bwd = subset.cv(regfit.bwd, data, 0.1, 15)

result=result.full
png('./plots/variable-selection-full.png')
as.matrix(result)
matplot(as.matrix(result), type = c("b"),pch=1,col = 2:4)
legend("topright", legend = c("C_p", "BIC", "AdjR^2"), col=2:4, pch=1)
dev.off()

result=result.fwd
png('./plots/variable-selection-fwd.png')
as.matrix(result)
matplot(as.matrix(result), type = c("b"),pch=1,col = 2:4)
legend("topright", legend = c("C_p", "BIC", "AdjR^2"), col=2:4, pch=1)
dev.off()


result=result.bwd
png('./plots/variable-selection-bwd.png')
as.matrix(result)*ㄲ
matplot(as.matrix(result), type = c("b"),pch=1,col = 2:4)
legend("topright", legend = c("C_p", "BIC", "AdjR^2"), col=2:4, pch=1)
dev.off()


png('./plots/variable-selection.png')
mat = as.matrix(cbind(as.matrix(result), t(ridge.result), t(lasso.result)))
matplot(mat, type = c("b"),pch=1,col = 2:6)
legend("topright", legend = c("C_p", "BIC", "AdjR^2", "Ridge", "Lasso"), col=2:6, pch=1)
dev.off()
