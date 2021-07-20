library(ProjectTemplate)
load.project()

logger <- logger()

info(logger, "Creating core COVID-19 dataset")
covid19 <- merge(observations, encounters, by.x = c("PATIENT", "ENCOUNTER"), by.y = c("PATIENT", "Id"))

info(logger, "Cleaning up data values and types")
covid19[, OBSERVATION_TIME := round_date(as_datetime(DATE), unit = "day")]
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

covid19[, TINDEX := time_length(OBSERVATION_TIME - ADMIT_TIME, unit = "day")]

info(logger, "Determine mortality cases")
## Who died and made these people boss?
outcomes <- patients[, list(Id, DEATHDATE)]
outcomes[, DEATH_TIME := round_date(as_datetime(DEATHDATE), unit = "day")]
covid19 <- merge(covid19, outcomes, by.x = "PATIENT", by.y = "Id", all.x = TRUE)
covid19[, OUTCOME := ifelse(is.na(DEATH_TIME), TRUE, FALSE)]
covid19[, DINDEX := time_length(DEATH_TIME - OBSERVATION_TIME, unit = "day")]
covid19[DINDEX > 9 | is.na(DINDEX), DINDEX := 10]

covid19.dt <- dcast(data = covid19, formula = PATIENT + TINDEX + DINDEX ~ CODE.x, fun.aggregate = mean, na.rm = TRUE, value.var = "VALUES")

## NAs beat NaNs here
is.nan.data.frame <- function(x) { do.call(cbind, lapply(x, is.nan)) }
covid19.dt[is.nan(covid19.dt)] <- NA

covid19.outcomes <- covid19.dt[, list(PATIENT, TINDEX, DINDEX)]
covid19.outcomes[, PINDEX := paste(PATIENT, "-", TINDEX, sep = "")]
covid19.outcomes <- covid19.outcomes[, list(PINDEX, DINDEX)]
info(logger, "Writing mortality data to outputs/outcomes.csv")
write_csv(covid19.outcomes, paste("outputs/outcomes.csv", sep = ""))
covid19.dt[, DINDEX := NULL]

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
    for(j in covid19_tmp[, TINDEX]) {
        covid19_subtmp <- covid19_tmp[TINDEX <= j]
        cols <- names(covid19_subtmp[, -"TINDEX"])
        cols_mask <- paste(cols, "_mask", sep = "")
        obs_count <- nrow(covid19_subtmp)
        covid19_subtmp <- covid19_subtmp[c(1:obs_count, rep(obs_count, max_obs - obs_count))]
        debug(logger, paste("Writing ", i, "-", j, " patient data to outputs/patients/", i, "-", j, ".csv", sep = ""))
        write_csv(covid19_subtmp, paste("outputs/patients/", i, "-", j, ".csv", sep = ""))
    }
}

