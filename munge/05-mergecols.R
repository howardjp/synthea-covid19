info(logger, "Reassembling the matix...and that really sounds cooler than it is")
covid19.dt <- merge(covid19.obvs.dt, covid19.meds.dt, by = c("PATIENT"), all.x = TRUE)
covid19.dt <- merge(covid19.dt, covid19.prcs.dt, by = c("PATIENT"), all.x = TRUE)
covid19.dt <- merge(covid19.dt, covid19.chrn.dt, by = "PATIENT", all.x = TRUE)
covid19.dt <- merge(covid19.dt, covid19.ptxs.dt, by.x = "PATIENT", by.y = "Id")

for(i in c(covid19.meds.names, covid19.prcs.names)) {
    covid19.dt[is.infinite(covid19.dt[, get(i)]), eval(i) := 0] 
}

covid19.dt <- covid19.dt[!is.na(TINDEX)]
## NAs beat NaNs here
is.nan.data.frame <- function(x) {
    do.call(cbind, lapply(x, is.na))
}
covid19.dt[is.nan(covid19.dt)] <- NA

info(logger, "Filling in zeros for missing values among meds and prcs")
covid19.dt.namelist <- (colSums(is.na(covid19.dt)) / nrow(covid19.dt)) > 1
covid19.meds.dt.names <- names(covid19.meds.dt)[-1]
covid19.meds.dt.names <- covid19.meds.dt.names[covid19.meds.dt.names %in% names(covid19.dt)]
covid19.prcs.dt.names <- names(covid19.prcs.dt)[-1]
covid19.prcs.dt.names <- covid19.prcs.dt.names[covid19.prcs.dt.names %in% names(covid19.dt)]
covid19.chrn.dt.names <- names(covid19.chrn.dt)[-1]
covid19.chrn.dt.names <- covid19.chrn.dt.names[covid19.chrn.dt.names %in% names(covid19.dt)]
covid19.dt.names <- c(covid19.meds.dt.names, covid19.prcs.dt.names, covid19.chrn.dt.names)
for(i in covid19.dt.names) {
    missingMedsPrcs <- as.vector(is.na(covid19.dt[, ..i]))
    covid19.dt[missingMedsPrcs, eval(i) := 0]
}


covid19.dt.bk <- covid19.dt
cache("covid19.dt")
cache("covid19.dt.bk")
