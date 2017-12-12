## install.packages('glmnet')
## install.packages('elasticnet')
library(glmnet)
library(elasticnet)

data = read.csv('./datasets/bankruptcy-datasets.csv', header=TRUE, row.names=NULL, sep = ";")
dim(data)
extract.data <- function (data, columns) {
    data = data[,!(names(data) %in% columns)]
    data = data[complete.cases(data),]
    return (data)
}
remove.noise <- function (data, colname, match) {
    matched = which(data[[colname]] == match)
    if (length(matched) > 0) {
        data = data[-matched,]
    }
    return(data)
}
convert.data <- function (data, columns) {
    tmp = data[,(names(data) %in% columns)]
    data = extract.data(data, columns)
    for (colname in names(tmp)) {
        new = rep(0, length(tmp[[colname]]))
        new[tmp[[colname]] == 'YES'] = 1
        new[tmp[[colname]] == 'yes'] = 1
        data = data.frame(data, new)
        clname = colnames(data)
        clname[length(clname)] = colname
        colnames(data) <- clname
    }
    return (data)
}
data = extract.data(data, c("YEAR","SOCIAL_CODE","PROVINCE_CODE"))
data = remove.noise(data, "AGE", -1)
data = convert.data(data, c("BANKRUPTCY", "DELAY_ACCOUNTS", "LINKED_GROUP", "AUDITED"))







x = model.matrix(BANKRUPTCY ~., data)[,-1]
y = data$BANKRUPTCY
grid = 10^seq(10, -2, length = 100) # lambda 생성
ridge.mod=glmnet(x, y,alpha=1, lambda=grid)
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

ridge.mod = glmnet(x[train,], y[train], alpha = 1, lambda=grid)

ridge.pred = predict(ridge.mod, s=4, newx=x[test,])

mean((ridge.pred-y.test)^2) # y가 로지스틱이므로 RSS가 아닌 확률오차로 변경할 것
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

cv.out = cv.glmnet(x[train,],y[train],alpha=0)

png('./plots/lasso-regression.png')
plot(cv.out)
dev.off()

