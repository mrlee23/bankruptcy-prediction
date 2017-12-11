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
data = convert.data(data, c("BANKRUPTCY", "DELAY_ACCOUNTS", "LINKED_GROUP", "AUDITED"))
