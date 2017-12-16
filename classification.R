source('./load.R')
library(MASS)
library(class)
attach(data)

train=sample(1:dim(data)[1], dim(data)[1]*0.5 ,replace=F)
test=-train
data.train=data[test,]
BANKRUPTCY.test=data$BANKRUPTCY[test]

# Logistic
glm.fit2=glm(BANKRUPTCY ~ SIZE + DEBT_COST + RETURN_EQUITY 
             + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR 
             + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED
             ,data=data,family=binomial,subset=train)
summary(glm.fit2)

glm.prob=predict(glm.fit2,data.train,type="response")
glm.pred=rep("NO",dim(data.train)[1])
glm.pred[glm.prob>.0715]="YES" # threshold 0.0715
table(glm.pred,BANKRUPTCY.test)
mean(glm.pred!=BANKRUPTCY.test)

glm.x = c()
glm.y = c()
for (x in seq(0, 0.5, by = 0.0001)) {
    glm.pred=rep("NO",dim(data.train)[1])
    glm.pred[glm.prob>x]="YES"
    glm.x = c(glm.x, x)
    glm.y = c(glm.y, mean(glm.pred!=BANKRUPTCY.test))
}
plot(glm.x, glm.y, xlab="Threshold", ylab="Error Rate", type="l")

# LDA
lda.fit=lda(BANKRUPTCY ~ SIZE + DEBT_COST + RETURN_EQUITY 
            + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR 
            + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED
            ,data=data,subset=train)

lda.pred=predict(lda.fit, data.train)
names(lda.pred)
summary(lda.fit)
lda.class=lda.pred$class
table(lda.class,BANKRUPTCY.test)
mean(lda.class!=BANKRUPTCY.test)

# QDA
qda.fit=qda(BANKRUPTCY ~ SIZE + DEBT_COST + RETURN_EQUITY 
            + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR 
            + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED
            ,data=data,subset=train)

qda.class=predict(qda.fit,data.train)$class
table(qda.class,BANKRUPTCY.test)
mean(qda.class!=BANKRUPTCY.test)

# KNN
train.X=cbind(SIZE,DEBT_COST,RETURN_EQUITY 
              ,RETURN_ASSETS,NUMBER_JUDICIAL_INCIDENCES_YEAR 
              ,SPENT_JUDICIAL_INCIDENCES_TOTAL,AUDITED)[train,]
test.X=cbind(SIZE,DEBT_COST,RETURN_EQUITY 
             ,RETURN_ASSETS,NUMBER_JUDICIAL_INCIDENCES_YEAR 
             ,SPENT_JUDICIAL_INCIDENCES_TOTAL,AUDITED)[test,]
train.BANKRUPTCY=BANKRUPTCY[train]

set.seed(1)

means=rep(0,30)
for (i in 1:30) {
  knn.pred=knn(train.X,test.X,train.BANKRUPTCY,k=i)
  print(table(knn.pred,BANKRUPTCY.test))
  mean=mean(knn.pred!=BANKRUPTCY.test)
  cat("k=",i,"->",mean,"\n")
  means[i]=mean;
}
min(means)
