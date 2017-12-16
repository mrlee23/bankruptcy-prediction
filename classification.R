source('./load.R')
attach(data)
plot(data,panel=panel.smooth)

train=sample(1:dim(data)[1], 1191 ,replace=F) #dim(data)[1]*0.7 =1669.5
test=-train
data.train=data[test,]
BANKRUPTCY.test=data$BANKRUPTCY[test]

#logistic

glm.fit2=glm(BANKRUPTCY ~ SIZE + DEBT_COST + RETURN_EQUITY 
             + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR 
             + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED
             ,data=data,family=binomial,subset=train)
summary(glm.fit2)

glm.prob=predict(glm.fit2,data.train,type="response")
glm.pred=rep("NO",dim(data.train)[1])
glm.pred[glm.prob>.0715]="YES" 
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

#lda
?predict.lda
library(MASS)
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
plot(lda.class)
#QDA
qda.fit=qda(BANKRUPTCY ~ SIZE + DEBT_COST + RETURN_EQUITY 
            + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR 
            + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED
            ,data=data,subset=train)

qda.class=predict(qda.fit,data.train)$class
table(qda.class,BANKRUPTCY.test)
mean(qda.class!=BANKRUPTCY.test)
plot(qda.class)

#KNN
library(class)

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
?table
plot(means)


#decision plot
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}

decisionplot(glm.fit,data)





















