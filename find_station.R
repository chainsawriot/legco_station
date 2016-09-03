require(rio)

dice2012 <- function(fname, ncand) {
    raw_data <- import(fname)
    meat <- raw_data[2:nrow(raw_data),1:(3+ncand)]
    meat <- meat[!is.na(meat[,1]) & nchar(meat[,1]) > 1,]
    colnames(meat) <- c("code", "cname", "ename", paste0("c", 1:ncand))
    return(meat)
}
dice2008 <- function(fname, ncand) {
    raw_data <- import(fname)
    meat <- raw_data[2:nrow(raw_data),1:(3+ncand)]
    meat <- meat[!is.na(meat[,1]) & stringr::str_detect(meat[,1], "^[A-Z][0-9]+"),]
    colnames(meat) <- c("code", "cname", "ename", paste0("c", 1:ncand))
    return(meat)
}

meat12 <- dice2012("cs_result_LC5.xls", 19)
meat08 <- dice2008("counting_results_LC5.xls", 10)
station16 <- readRDS("lc5_2016.RDS")
goodstation <- intersect(intersect(meat12$cname, meat08$cname), station16[,2])

greedy <- function(x, goodstation, iteration = 10) {
    meat <- x[x$cname %in% goodstation,]
    finalvotes <- apply(x[,4:ncol(x)], 2, sum)
    bag <- c()
    cor1 <- sapply(1:nrow(meat), function(x) cor(apply(meat[x,4:ncol(meat)], 2, sum), finalvotes))
    bag <- append(bag, which.max(cor1))
    for (i in 1:(iteration-1)) {
        cor2 <- sapply(setdiff(1:nrow(meat), bag), function(x) cor(apply(meat[c(bag, x), 4:ncol(meat)], 2, sum), finalvotes))
        bag <- append(bag, setdiff(1:nrow(meat), bag)[which.max(cor2)])
    }
    return(meat$cname[bag])
}
    
evaluate <- function(stations, y) {
    finalvotes <- apply(y[,4:ncol(y)], 2, sum)
    xvotes <- apply(y[y$cname %in% stations, 4:ncol(y)], 2, sum)
    return(cor(finalvotes, xvotes))
}

plot(apply(meat12[,4:ncol(meat12)], 2, sum), apply(meat12[meat12$cname %in% meat[bag,2],4:ncol(meat12)], 2, sum))
