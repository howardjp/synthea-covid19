NASDROPTHRESHOLDLIST <- c(0.20, 0.25, 0.30)
VARDROPTHRESHOLDLIST <- c(0.01, 0.05, 0.10)
COVDROPTHEREHOLDLIST <- c(0.75, 0.85, 0.95) 

patient_list <- sort(unique(covid19.dt[, PATIENT]))

threshold_grid <- expand.grid(nasdropthreshold = NASDROPTHRESHOLDLIST,
                              vardropthreshold = VARDROPTHRESHOLDLIST,
                              covdropthreshold = COVDROPTHEREHOLDLIST) 
threshold_grid <- data.table(threshold_grid)

for(i in 1:nrow(threshold_grid)) {
    covid19.dt <- covid19.dt.bk
    patient_list <- sort(unique(covid19.dt[, PATIENT]))

    NASDROPTHRESHOLD <- threshold_grid[i, nasdropthreshold]
    VARDROPTHRESHOLD <- threshold_grid[i, vardropthreshold]
    COVDROPTHRESHOLD <- threshold_grid[i, covdropthreshold]

    info(logger, paste("Starting assembly of nasdropthreshold = ", 
                        NASDROPTHRESHOLD, ", vardropthreshold = ",
                        VARDROPTHRESHOLD, ", covdropthreshold = ",
                        COVDROPTHRESHOLD, sep = ""))

    ## Remove columns with more than x% missing
    covid19.dt.droplist <- (colSums(is.na(covid19.dt)) / nrow(covid19.dt)) > NASDROPTHRESHOLD
    covid19.dt.droplist[c("PATIENT", "TINDEX", "DINDEX", "OUTCOME")] <- FALSE
    info(logger, paste("Dropping", sum(covid19.dt.droplist), "of", length(covid19.dt.droplist), "due to missing values"))
    covid19.dt <- covid19.dt[, !covid19.dt.droplist, with = FALSE]

    ## Remove columns with zero variance, but watch this neat trick...
    covid19.dt.namelist <- (colSums(is.na(covid19.dt)) / nrow(covid19.dt)) > 1
    covid19.obvs.dt.names <- names(covid19.obvs.dt)[-c(1:4)]
    covid19.obvs.dt.names <- covid19.obvs.dt.names[covid19.obvs.dt.names %in% names(covid19.dt)]
    covid19.meds.dt.names <- names(covid19.meds.dt)[-1]
    covid19.meds.dt.names <- covid19.meds.dt.names[covid19.meds.dt.names %in% names(covid19.dt)]
    covid19.prcs.dt.names <- names(covid19.prcs.dt)[-1]
    covid19.prcs.dt.names <- covid19.prcs.dt.names[covid19.prcs.dt.names %in% names(covid19.dt)]
    covid19.chrn.dt.names <- names(covid19.chrn.dt)[-1]
    covid19.chrn.dt.names <- covid19.chrn.dt.names[covid19.chrn.dt.names %in% names(covid19.dt)]
    covid19.dt.names <- c(covid19.obvs.dt.names, covid19.meds.dt.names, covid19.prcs.dt.names, covid19.chrn.dt.names)
    covid19.dt.droplist <- !(apply(covid19.dt[, ..covid19.dt.names], 2, var, na.rm = TRUE) > 0)
    covid19.dt.droplist <- names(covid19.dt.droplist[covid19.dt.droplist]) 
    covid19.dt.namelist[covid19.dt.droplist] <- TRUE
    info(logger, paste("Dropping", sum(covid19.dt.namelist), "of", length(covid19.dt.namelist), "due to zero variance"))
    covid19.dt <- covid19.dt[, !covid19.dt.namelist, with = FALSE]

    ## Remove columns with near zero variance, but watch this neat trick again...
    covid19.dt.namelist <- (colSums(is.na(covid19.dt)) / nrow(covid19.dt)) > 1
    covid19.obvs.dt.names <- names(covid19.obvs.dt)[-c(1:4)]
    covid19.obvs.dt.names <- covid19.obvs.dt.names[covid19.obvs.dt.names %in% names(covid19.dt)]
    covid19.meds.dt.names <- names(covid19.meds.dt)[-1]
    covid19.meds.dt.names <- covid19.meds.dt.names[covid19.meds.dt.names %in% names(covid19.dt)]
    covid19.prcs.dt.names <- names(covid19.prcs.dt)[-1]
    covid19.prcs.dt.names <- covid19.prcs.dt.names[covid19.prcs.dt.names %in% names(covid19.dt)]
    covid19.chrn.dt.names <- names(covid19.chrn.dt)[-1]
    covid19.chrn.dt.names <- covid19.chrn.dt.names[covid19.chrn.dt.names %in% names(covid19.dt)]
    covid19.dt.names <- c(covid19.obvs.dt.names, covid19.meds.dt.names, covid19.prcs.dt.names, covid19.chrn.dt.names)
    covid19.dt.droplist <- apply(covid19.dt[, ..covid19.dt.names], 2, var, na.rm = TRUE) < VARDROPTHRESHOLD
    covid19.dt.droplist <- names(covid19.dt.droplist[covid19.dt.droplist]) 
    covid19.dt.namelist[covid19.dt.droplist] <- TRUE
    info(logger, paste("Dropping", sum(covid19.dt.namelist), "of", length(covid19.dt.namelist), "due to near-zero variance"))
    covid19.dt <- covid19.dt[, !covid19.dt.namelist, with = FALSE]

    ## Remove highly-correlated columns, but we are gonna reuse the trick...
    covid19.dt.namelist <- (colSums(is.na(covid19.dt)) / nrow(covid19.dt)) > 1
    covid19.obvs.dt.names <- names(covid19.obvs.dt)[-c(1:4)]
    covid19.obvs.dt.names <- covid19.obvs.dt.names[covid19.obvs.dt.names %in% names(covid19.dt)]
    covid19.meds.dt.names <- names(covid19.meds.dt)[-1]
    covid19.meds.dt.names <- covid19.meds.dt.names[covid19.meds.dt.names %in% names(covid19.dt)]
    covid19.prcs.dt.names <- names(covid19.prcs.dt)[-1]
    covid19.prcs.dt.names <- covid19.prcs.dt.names[covid19.prcs.dt.names %in% names(covid19.dt)]
    covid19.dt.names <- c(covid19.obvs.dt.names, covid19.meds.dt.names, covid19.prcs.dt.names)
    covid19.dt.cor <- cor(covid19.dt[, ..covid19.dt.names], use = "na.or.complete")
    covid19.dt.fc <- findCorrelation(covid19.dt.cor, cutoff = COVDROPTHRESHOLD)
    covid19.dt.namelist[covid19.dt.names[covid19.dt.fc]] <- TRUE
    info(logger, paste("Dropping", sum(covid19.dt.namelist), "of", length(covid19.dt.namelist), "due to excessive covariance"))
    covid19.dt <- covid19.dt[, !covid19.dt.namelist, with = FALSE]

    filename <- paste("output/simple-", format(NASDROPTHRESHOLD, nsmall = 2), "-", format(VARDROPTHRESHOLD, nsmall = 2), "-", format(COVDROPTHRESHOLD, nsmall = 2), ".json", sep = "")
    make_json(covid19.dt, patient_list, filename)
}