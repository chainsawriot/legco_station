require(rvest)
require(methods)

grab_station <- function(x) {
    read_html(paste0("http://www.elections.gov.hk/legco2016/chi/poll_", x,".html")) %>% html_node("table.contents2") %>% html_table -> temptable
    temptable[,1] <- gsub("\\*", "", temptable[,1])
    return(temptable[,c(1,2)])
}

stations <- list(c("a", "b", "c", "d"), c("e", "f", "g"), c("h", 'j'), c('k', 'l', 'm', 's', 't'), c("n", "p", "q", "r"))
for (i in 1:5) {
    lc5_2016 <- plyr::ldply(stations[[i]], grab_station)
    saveRDS(lc5_2016, paste0('lc', i, '_2016.RDS'))
}

for (i in 1:5) {
    download.file(paste0('http://www.elections.gov.hk/legco2012/eng/cs_result_LC',i,'.xls'), paste0('cs_result_LC',i,'.xls'))
    download.file(paste0('http://www.elections.gov.hk/legco2008/eng/result/download/counting_results_LC',i,'.xls'), paste0('counting_results_LC',i,'.xls'))
}
