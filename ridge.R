## install.packages('elasticnet')
library(elasticnet)
source('./load.R')
ls()

x = model.matrix(BANKRUPTCY ~., data)[,-1]
y = data$BANKRUPTCY
grid = 10^seq(10, -2, length = 100) # lambda 생성
ridge.mod=glmnet(x, y,alpha=0, lambda=grid)
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

ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda=grid)

ridge.pred = predict(ridge.mod, s=4, newx=x[test,])

mean((ridge.pred-y.test)^2) # y가 로지스틱이므로 RSS가 아닌 확률오차로 변경할 것
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

cv.out = cv.glmnet(x[train,],y[train],alpha=0)

png('./plots/ridge-regression.png')
plot(cv.out)
dev.off()


# subset selection
