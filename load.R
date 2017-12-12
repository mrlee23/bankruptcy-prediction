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
print(dim(data))
data = extract.data(data, c("YEAR","SOCIAL_CODE","PROVINCE_CODE"))
data = remove.noise(data, "AGE", -1)
data = convert.data(data, c("DELAY_ACCOUNTS", "LINKED_GROUP", "AUDITED"))
print(dim(data))
