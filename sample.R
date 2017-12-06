data = read.csv('./datasets/bankruptcy-datasets.csv', header=TRUE, row.names=NULL, sep = ";")
dim(data)
attach(data)
extract.data <- function (data, columns) {
    data = data[,!(names(data) %in% columns)]
    data = data[complete.cases(data),]
    return (data)
}
data = extract.data(data, c(""))
print(summary(glm(BANKRUPTCY ~ .,  data=data, family=binomial)))
