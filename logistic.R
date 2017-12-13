source('./load.R')

glm.err <- function (pred, res, thresh) {
    glm.pred = rep("NO", length(res))
    glm.pred[pred>thresh] = "YES"
    return(1-mean(glm.pred == res))
    }
glm.plot <- function (pred, res, range) {
    x = c()
    y = c()
    for (thresh in range) {
        x = c(x, thresh)
        y = c(y, glm.err(pred, res, thresh))
    }
    plot(x=x, y=y, xlab="threshold", ylab="error rate", type="l")
}

# 1. 모든 변수를 넣고 Logistic 분석
glm.fit=glm(BANKRUPTCY~.,data=data,family=binomial)
data.len = length(data$BANKRUPTCY)
data.res = data$BANKRUPTCY
glm.probs=predict(glm.fit, data=data, type="response")
glm.pred=rep("NO", data.len)
glm.pred[glm.probs>.5]="YES"

print(summary(glm.fit))
print(coef(summary(glm.fit)))
print(mean(glm.pred==data.res))

sample(0, 5)
seq(8, -8, length=100)

glm.err(glm.probs, data.res, 0.5)
png('./plots/logistic.png')
glm.plot(glm.probs, data.res, seq(0, 1, length=100))
dev.off()
length(data[sample(1:data.len, floor(data.len/2)),]$BANKRUPTCY)
