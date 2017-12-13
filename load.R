data = read.csv('./datasets/bankruptcy-datasets.csv', header=TRUE, row.names=NULL, sep = ";")
#' Exclude Data
#' 
#' 각 열의 이름에 해당하는 데이터를 제거하고 각 행의 데이터에 NA가 있을 경우 해당하는 행을 제거한 후 데이터를 반환한다.
#'
#' @param data 선택된 열을 제거할 data frame
#' @param columns 제거할 열의 이름이 나열된 vector
#'
#' @return 선택된 열과 NA를 가지는 행이 모두 제거된 data frame
exclude.data <- function (data, columns) {
    data = data[,!(names(data) %in% columns)]
    data = data[complete.cases(data),]
    return (data)
}

#' Remove Noise
#'
#' 열 이름에 해당하는 데이터 중 특정 데이터 가진 행을 데이터에서 제거한다.
#'
#' @param data data frame
#' @param colname 비교할 데이터가 담긴 열 이름
#' @param match 데이터를 비교할 값, 이 값과 일치하는 데이터를 가진 행은 제거된다.
#'
#' @return 매칭된 데이터 행이 제거된 data frame
remove.noise <- function (data, colname, match) {
    matched = which(data[[colname]] == match)
    if (length(matched) > 0) {
        data = data[-matched,]
    }
    return(data)
}

#' Convert Data
#'
#' YES/NO로 나뉘는 질적변수의 데이터를 1/0으로 변환한다.
#'
#' @param data data frame
#' @param columns 변환할 열의 이름이 나열된 vector
#'
#' @return 변환된 열을 새로운 열로 가지는 data frame
convert.data <- function (data, columns) {
    for (colname in columns) {
        tmp = rep(0, length(data[[colname]]))
        tmp[grep("YES", data[[colname]], ignore.case = TRUE)] = 1
        data[[colname]] = tmp
    }
    return (data)
}
print(dim(data))
## data = exclude.data(data, c())
## data = exclude.data(data, c("YEAR","SOCIAL_CODE","PROVINCE_CODE", "AUDITORS_OPINION"))
data = exclude.data(data, c("SOCIAL_CODE", "PROVINCE_CODE"))
data = remove.noise(data, "AGE", -1)
if (length(data$BANKRUPTCY) %% 2 == 1) {
    data = data[-c(1),]
}
## data = convert.data(data, c("BANKRUPTCY")) # LDA, QDA 분석 시 해당 코드를 사용할 것.
print(dim(data))

# 변수 테스트
## data = exclude.data(data, c("PROVINCE_CODE"
##                            ## ,"SIZE"
##                            ## ,"SOCIAL_CODE"
##                            ## ,"LINKED_GROUP"
##                            ## ,"DELAY_ACCOUNTS"
##                            ## ,"AUDITED"
##                            ## ,"AUDITORS_OPINION"
##                            ## ,"YEAR"
##                            ## ,"NUMBER_EMPLOYEES"
##                            ## ,"AGE"
##                            ## ,"NUMBER_PARTNERS"
##                            ## ,"CHANGES_LOCATION"
##                            ## ,"DEBT_STRUCTURE"
##                            ## ,"DEBT_COST"
##                            ## ,"DEBT_PAYING_AVAILABILITY"
##                            ## ,"DEBT_RATIO"
##                            ## ,"WORKING_CAPITAL"
##                            ## ,"WARRANTY"
##                            ## ,"OPERATING_INCOME_MARGIN"
##                            ## ,"RETURN_OPERATING_ASSETS"
##                            ## ,"RETURN_EQUITY"
##                            ## ,"RETURN_ASSETS"
##                            ## ,"STOCK_TURNOVER"
##                            ## ,"ASSET_TURNOVER"
##                            ## ,"RECEIVABLE_TURNOVER"
##                            ## ,"ASSET_ROTATION"
##                            ## ,"FINANCIAL_SOLVENCY"
##                            ## ,"ACID_TEST"
##                            ## ,"NUMBER_JUDICIAL_INCIDENCES_TOTAL"
##                            ## ,"NUMBER_JUDICIAL_INCIDENCES_YEAR"
##                            ## ,"SPENT_JUDICIAL_INCIDENCES_TOTAL"
##                            ## ,"SPENT_JUDICIAL_INCIDENCES_YEAR"
##                            ## ,"NUMBER_SERIOUS_INCIDENCES"
##                             ))
