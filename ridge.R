## install.packages('elasticnet')
source('./load.R')
data = convert.data(data, c("BANKRUPTCY"))
library(elasticnet)
library(glmnet)
ls()

x = model.matrix(BANKRUPTCY ~., data)[,-1]
y = data$BANKRUPTCY
grid = 10^seq(10, -2, length = 100) # lambda 생성
ridge.mod=glmnet(x, y,alpha=0, lambda=grid, family="binomial")
dim(coef(ridge.mod))

# ridge 결과값 확인
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # -1은 Intercept를 제외

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

train=sample(1:nrow(x), nrow(x)/2) # 트레이닝 데이터 생성
test = (-train) # 테스트 데이터
y.test=y[test]

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda=grid, family="binomial")

ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
ridge.pred
y.test
mean((ridge.pred-y.test)^2) # y가 로지스틱이므로 RSS가 아닌 확률오차로 변경할 것
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
?cv.glmnet
mean((ridge.pred-y.test)^2)

cv.out = cv.glmnet(x[train,],y[train],alpha=0)

png('./plots/ridge-regression.png')
plot(cv.out)
dev.off()

# lasso

lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda=grid)

lasso.pred = predict(lasso.mod, s=4, newx=x[test,])

mean((lasso.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)

lasso.pred=predict(lasso.mod, s=1e10, newx=x[test,])
mean((lasso.pred-y.test)^2)

cv.out = cv.glmnet(x[train,],y[train],alpha=0)

png('./plots/lasso-regression.png')
plot(cv.out)
dev.off()



##########################
error.glmnet <- function (x, y, train, test, alpha, lmabda) {
    mod = glmnet(x[train,], y[train], alpha=alpha, lambda=lmabda, family="binomial")
    pred = 
}
error.cv <- function (formula, data, thresh, seed.num) {
    data.len = length(data[,1])
    set.seed(seed.num)
    train = sample(1:data.len, data.len/2)
    train.data = data[train,]
    test.data = data[-train,]

    return(error.glm(formula, train.data, test.data, thresh))
}
cross.validation <- function (data, thresh, seed.num) {
    x = model.matrix(BANKRUPTCY ~., data)[,-1]
    y = data$BANKRUPTCY
    grid = 10^seq(10, -2, length = 300) # lambda 생성
}

x = model.matrix(BANKRUPTCY ~., data)[,-1]
y = data$BANKRUPTCY
grid = 10^seq(10, -2, length = 300) # lambda 생성
ridge.cv = cv.glmnet(x, y, alpha=0, lambda=grid, family="binomial")
ridge.fit = glmnet(x, y, alpha=0, family="binomial")
predict(ridge.fit, s=ridge.cv$lambda.min, type="coefficients")




##################### lasso
x = model.matrix(BANKRUPTCY ~., data)[,-1]
y = data$BANKRUPTCY
grid = 10^seq(0, -5, length = 500) # lambda 생성
lasso.cv = cv.glmnet(x, y, alpha=1, lambda=grid, family="binomial")
lasso.fit = glmnet(x, y, alpha=1, lambda=grid, family="binomial")
lasso.pred = predict(lasso.fit, s=lasso.cv$lambda.min, type="coefficients")
lasso.pred
lasso.cv$lambda.min
plot(lasso.cv)
names(lasso.pred[,1])

grid = 10^seq(2, -2, length = 1000) # lambda 생성
lasso.cv$lambda.min # 0.001071593
cross.validation(names(lasso.pred[,1]), data, 0.5, 15)
cross.validation(c("YEAR","SIZEmedium","SIZEsmall","AGE","LINKED_GROUPyes","WORKING_CAPITAL","RETURN_EQUITY","RETURN_ASSETS","ASSET_TURNOVER","FINANCIAL_SOLVENCY","NUMBER_JUDICIAL_INCIDENCES_TOTAL","NUMBER_JUDICIAL_INCIDENCES_YEAR","SPENT_JUDICIAL_INCIDENCES_TOTAL","AUDITEDyes","AUDITORS_OPINIONpositive"), data, 0.5, 15)
#          cv      loocv   X10.fold
#1 0.02602855 0.02651779 0.02654115

grid = 10^seq(-1, -4, length = 500) # lambda 생성
cross.validation(c("YEAR","SIZEmedium","SIZEsmall","NUMBER_EMPLOYEES","AGE","LINKED_GROUPyes","NUMBER_PARTNERS","WORKING_CAPITAL","OPERATING_INCOME_MARGIN","RETURN_EQUITY","RETURN_ASSETS","STOCK_TURNOVER","ASSET_TURNOVER","FINANCIAL_SOLVENCY","NUMBER_JUDICIAL_INCIDENCES_TOTAL","NUMBER_JUDICIAL_INCIDENCES_YEAR","SPENT_JUDICIAL_INCIDENCES_TOTAL","AUDITEDyes","AUDITORS_OPINIONnegative","AUDITORS_OPINIONpositive"), data, 0.5, 15)
#          cv      loocv   X10.fold
#1 0.02518892 0.02543646 0.02584154

grid = 10^seq(0, -3, length = 500) # lambda 생성


grid = 10^seq(0, -5, length = 500) # lambda 생성
cross.validation(c("YEAR","SIZEmedium", "AGE","LINKED_GROUPyes","WORKING_CAPITAL","RETURN_EQUITY","RETURN_ASSETS","ASSET_TURNOVER","NUMBER_JUDICIAL_INCIDENCES_TOTAL","NUMBER_JUDICIAL_INCIDENCES_YEAR","SPENT_JUDICIAL_INCIDENCES_TOTAL","AUDITEDyes","AUDITORS_OPINIONpositive"), data, 0.5, 15)


grid = 10^seq(3, 0, length = 2000) # lambda 생성

grid = seq(0.001, 1, length = 500)
