logger <- logger()

make_json <- function(covid19.data.dt, patient_list, filename) {
    covid.data <- list()
    covid.data$time_index <- sort(unique(covid19.dt[, TINDEX]))
    covid.data$info <- list()
    covid.data$data <- list()
    covid.data$outcome <- list()

    for(i in patient_list) {
        covid19.dt.i <- covid19.data.dt[PATIENT == i]
        setorderv(covid19.dt.i, "TINDEX")
        outcome.i <- covid19.dt.i[1, OUTCOME]
        covid.data$info[[i]] <- list()
        for(j in covid19.dt.i[, TINDEX]) {
            id <- uuid::UUIDgenerate()
            covid19.dt.i.j <- covid19.dt.i[TINDEX <= j]
            dindex.i.j <- covid19.dt.i[, max(TINDEX)] - covid19.dt.i.j[, max(TINDEX)]
            debug(logger, paste("Patient:", i, "Time:", j, "Outcome:", outcome.i, "Time Remaining:", dindex.i.j))
            covid19.dt.i.j <- covid19.dt.i.j[, -c("PATIENT", "DINDEX", "OUTCOME")]
            covid.data$info[[i]][[id]] <- list(time = j)
            covid.data$outcome[[id]] <- list(outcome = outcome.i, time = dindex.i.j)
            covid.data$data[[id]] <- as.matrix(covid19.dt.i.j)
        }
    }

    info(logger, "Converting to JSON")
    covid.data.json <- toJSON(covid.data, digits = NA, pretty = TRUE, na = "null", auto_unbox = TRUE)

    info(logger, paste("Writing JSON output to", filename))
    write(covid.data.json, filename)

    info(logger, "Compressing JSON output file")
    system(paste("bzip2 -9", filename)) 
}