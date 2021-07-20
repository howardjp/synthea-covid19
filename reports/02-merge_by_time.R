logger <- logger()

info(logger, "Creating core COVID-19 dataset")
covid19 <- merge(observations, encounters, by.x = c("PATIENT", "ENCOUNTER"), by.y = c("PATIENT", "Id"))

info(logger, "Cleaning up data values and types")
covid19[, OBSERVATION_TIME := round_date(as_datetime(DATE), unit = "4 hours")]
covid19[, ENCOUNTERCLASS := as.factor(ENCOUNTERCLASS)]
covid19[, UNITS := as.factor(UNITS)]
covid19[, VALUES := as.numeric(VALUE)]

## Now we have to fixup a bunch of non-numerics
covid19[is.na(VALUES) & CODE.x == "80271-0", VALUES := 1]
covid19[is.na(VALUES) & CODE.x == "88040-1", VALUES := 1]
covid19[is.na(VALUES) & CODE.x == "94531-1" & VALUE == "Not detected (qualifier value)", VALUES := 0]
covid19[is.na(VALUES) & CODE.x == "94531-1" & VALUE == "Detected (qualifier value)", VALUES := 1]
covid19[is.na(VALUES) & CODE.x == "72166-2", VALUES := as.numeric(as.factor(VALUE))]

info(logger, "Eliminating non-emergency and non-inpatient cases")
covid19 <- covid19[ENCOUNTERCLASS == "emergency" | ENCOUNTERCLASS == "inpatient"]
covid19[, ADMIT_TIME := min(OBSERVATION_TIME), by = PATIENT]

info(logger, "Removing all cases with an admission date before March 1, 2020")
covid19 <- covid19[ADMIT_TIME >= "2020-03-01"]

covid19 <- covid19[, TINDEX := time_length(OBSERVATION_TIME - ADMIT_TIME, unit = "day"), by = ENCOUNTER]


covid19.dt <- dcast(data = covid19, formula = PATIENT + TINDEX ~ CODE.x, fun.aggregate = mean, na.rm = TRUE, value.var = "VALUES")

## NAs beat NaNs here
is.nan.data.frame <- function(x) { do.call(cbind, lapply(x, is.nan)) }
covid19.dt[is.nan(covid19.dt)] <- NA

info(logger, "Determine mortality cases")
## Who died and made these people boss?
outcomes <- data.table(PATIENT = encounters[DESCRIPTION == "Death Certification", PATIENT], OUTCOME = 0)
covid19.dt <- merge(covid19.dt, outcomes, by = "PATIENT", all.x = TRUE)
covid19.dt[is.na(OUTCOME), OUTCOME := 1]
covid19.dt[, OUTCOME := as.factor(OUTCOME)]

covid19.outcomes <- covid19.dt[, list(PATIENT, OUTCOME)]
covid19.outcomes <- unique(covid19.outcomes)
info(logger, "Writing mortality data to outputs/outcomes.csv")
write_csv(covid19.outcomes, paste("outputs/outcomes.csv", sep = ""))
covid19.dt[, OUTCOME := NULL]

patient_list <- unique(covid19.dt[, PATIENT])

mask_maker <- function(x) {
    cumsum(!is.na(x))
}

max_obs <- max(covid19.dt[, .N, by = PATIENT][, N])

info(logger, "Writing patient data to outputs/patients")
for(i in patient_list) {
    covid19_tmp <- covid19.dt[PATIENT == i]
    covid19_tmp[, PATIENT := NULL]
    setorderv(covid19_tmp, "TINDEX")
    cols <- names(covid19_tmp[, -"TINDEX"])
    cols_mask <- paste(cols, "_mask", sep = "")
    covid19_tmp[, (cols_mask) := lapply(.SD, mask_maker), .SDcols = cols]
    obs_count <- nrow(covid19_tmp)
    covid19_tmp <- covid19_tmp[c(1:obs_count, rep(obs_count, max_obs - obs_count))]
    debug(logger, paste("Writing ", i, " patient data to outputs/patients/", i, ".csv", sep = ""))
    write_csv(covid19_tmp, paste("outputs/patients/", i, ".csv", sep = ""))
}
