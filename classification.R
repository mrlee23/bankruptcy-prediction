library(MASS)
source('./load.R')
train = sample(1:nrow(data), nrow(data)/2) # 트레이닝 데이터 생성
test = (-train)
train.data = data[train,]
test.data = data[test,]

names(data)

lda.fit = lda(BANKRUPTCY ~ ., data=data, subset=train)
lda.pred = predict(lda.fit, test.data, type="response")
mean(lda.pred$class != test.data$BANKRUPTCY) # 0.03187919

lda.fit = lda(BANKRUPTCY ~ DEBT_COST, data=data, subset=train)
lda.pred = predict(lda.fit, test.data, type="response")
mean(lda.pred$class != test.data$BANKRUPTCY) # 0.01845638

qda.fit = qda(BANKRUPTCY ~ DEBT_COST, data=data, subset=train)
qda.pred = predict(qda.fit, test.data, type="response")
mean(qda.pred$class != test.data$BANKRUPTCY) # 0.6887584

lda.fit = lda(BANKRUPTCY ~ AGE + 	DEBT_COST + RETURN_EQUITY + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED, data=data, subset=train)
lda.pred = predict(lda.fit, test.data, type="response")
mean(lda.pred$class != test.data$BANKRUPTCY) # 0.03104027

qda.fit = qda(BANKRUPTCY ~ AGE + 	DEBT_COST + RETURN_EQUITY + RETURN_ASSETS + NUMBER_JUDICIAL_INCIDENCES_YEAR + SPENT_JUDICIAL_INCIDENCES_TOTAL + AUDITED, data=data, subset=train)
qda.pred = predict(qda.fit, test.data, type="response")
mean(qda.pred$class != test.data$BANKRUPTCY) # 0.04949664
