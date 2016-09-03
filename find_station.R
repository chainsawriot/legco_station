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

greedy <- function(x, goodstation, best = 0.1) {
    iteration <- ceiling(length(goodstation) * best)
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
    
evaluate <- function(stations, y, plot = FALSE) {
    finalvotes <- apply(y[,4:ncol(y)], 2, sum)
    xvotes <- apply(y[y$cname %in% stations, 4:ncol(y)], 2, sum)
    if (plot) {
        plot(finalvotes, xvotes, main = round(cor(finalvotes, xvotes), 3))
    }
    return(cor(finalvotes, xvotes))
}

experiment <- function(lc = 5, s12 = 19, s08 = 10) {
    meat12 <- dice2012(paste0("cs_result_LC",lc,".xls"), s12)
    meat08 <- dice2008(paste0("counting_results_LC", lc, ".xls"), s08)
    station16 <- readRDS(paste0("lc", lc, "_2016.RDS"))
    goodstation <- intersect(intersect(meat12$cname, meat08$cname), station16[,2])
    bstation12 <- greedy(meat12, goodstation)
    bstation08 <- greedy(meat08, goodstation)
    #print("best12")
    #print(bstation12, TRUE)
    #print("best08")
    #print(bstation08, TRUE)
    par(mfrow=c(2,1))
    evaluate(bstation12, meat08)
    evaluate(bstation08, meat12)
    return(list(bstation12, bstation08))
}

print("HKI")
experiment(1, 14, 10)
print("KWW")
experiment(2, 9, 13)
print("KWE")
experiment(3, 9, 6)
print("NTW")
experiment(4, 16, 14)
print("NTE")
experiment(5, 19, 10)
