info(logger, "Starting assembly of infogain-limited dataset")
covid19.dt <- covid19.dt.bk

namelist <- c("PATIENT", "DINDEX", "TINDEX", "AGE", "GENDER", "ETHNICITY", "asian", "black", "native", "other", "white", "M", "S")
weights <- information.gain(OUTCOME ~ ., covid19.dt[, -..namelist])
subset <- cutoff.k(weights, 16)

namelist <- c("PATIENT", "TINDEX", "DINDEX", "OUTCOME", "AGE", "GENDER", "ETHNICITY", "asian", "black", "native", "other", "white", "M", "S")
namelist <- c(namelist, subset)
info(logger, paste("Dropping", ncol(covid19.dt) - length(namelist), "of", ncol(covid19.dt), "due to excessive covariance"))

filename <- "output/infogain.json"
patient_list <- sort(unique(covid19.dt[, PATIENT]))

make_json(covid19.dt, patient_list, filename)
