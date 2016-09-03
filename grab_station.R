require(rvest)

grab_station <- function(x) {
    read_html(paste0("http://www.elections.gov.hk/legco2016/chi/poll_", x,".html")) %>% html_node("table.contents2") %>% html_table -> temptable
    temptable[,1] <- gsub("\\*", "", temptable[,1])
    return(temptable[,c(1,2)])
}

lc5_2016 <- plyr::ldply(c("n", "p", "q", "r"), grab_station)
saveRDS(lc5_2016, 'lc5_2016.RDS')
