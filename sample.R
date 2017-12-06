data = read.csv('Bankruptcy_Spain_98-03_(dot real separator).csv', header=TRUE, row.names=NULL, sep = ";")
dim(data)
attach(data)
extract.data <- function (data, columns) {
    data = data[,!(names(data) %in% columns)]
    data = data[complete.cases(data),]
    return (data)
}
data = extract.data(data, c(""))
summary(glm(BANKRUPTCY ~ DEBT_RATIO,  data=data, family=binomial))
