library(ProjectTemplate)
library("readr")
library("dplyr")
library("jsonlite")
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
covid19[is.na(DINDEX), DINDEX := -DINDEX]

covid19.dt <- dcast(data = covid19, formula = PATIENT + TINDEX + DINDEX ~ CODE.x, fun.aggregate = mean, na.rm = TRUE, value.var = "VALUES")

## NAs beat NaNs here
is.nan.data.frame <- function(x) { do.call(cbind, lapply(x, is.nan)) }
covid19.dt[is.nan(covid19.dt)] <- NA

covid19.outcomes <- covid19.dt[, list(PATIENT, TINDEX, DINDEX)]
covid19.outcomes[, PINDEX := paste(PATIENT, "-", TINDEX, sep = "")]
covid19.outcomes <- covid19.outcomes[, list(PINDEX, DINDEX)]
info(logger, "Writing mortality data to outputs/outcomes.csv")

patient_list <- unique(covid19.dt[, PATIENT])

mask_maker <- function(x) {
    cumsum(!is.na(x))
}

max_obs <- max(covid19.dt[, .N, by = PATIENT][, N]) 

info(logger, "Writing patient data to synthea/full.ts")

classVals <- sort(unique(covid19.outcomes[, DINDEX]))

dimens <- ncol(covid19.dt) - 1

tsfile <- "synthea/full.ts"

write_lines("@problemName COVID-19 Survival", tsfile)
write_lines("@timeStamps false", tsfile, append = TRUE)
write_lines("@missing false", tsfile, append = TRUE)
write_lines("@univariate false", tsfile, append = TRUE)
write_lines(paste("@dimensions", dimens) , tsfile, append = TRUE)
write_lines("@equalLength false", tsfile, append = TRUE)
write_lines("@equalLength false", tsfile, append = TRUE)
write_lines(paste("@classLabel true", paste(classVals, collapse = " ")), tsfile, append = TRUE)
write_lines("@data", tsfile, append = TRUE)

csvData = "foo"
for(i in patient_list) {
    covid19_tmp <- covid19.dt[PATIENT == i]
    covid19_tmp[, PATIENT := NULL]
    covid19_tmp[, DINDEX := NULL]
    setorderv(covid19_tmp, "TINDEX")
    for(j in covid19_tmp[, TINDEX]) {
        rm(csvData)
        covid19_subtmp <- covid19_tmp[TINDEX <= j]
        obs_count <- nrow(covid19_subtmp)
        covid19_subtmp <- covid19_subtmp[c(1:obs_count, rep(obs_count, max_obs - obs_count))]
        covid19_matrix <- as.matrix(covid19_subtmp)
        debug(logger, paste("Writing ", i, "-", j, " patient data", sep = ""))
        pidx <- paste(i, "-", j, sep = "")
        write.table(t(covid19_matrix), file = textConnection("csvData", "w"),
            col.names = FALSE, row.names = FALSE, sep = ",", na = "?")
        tsentry <- paste(csvData, sep = ":", collapse = ":")
        tsentry <- paste(tsentry, covid19.outcomes[PINDEX == pidx, DINDEX], sep = ":")
        write_lines(tsentry, tsfile, append = TRUE)
    }
}

info(logger, "Writing patient data to synthea/abbrev.ts")

covid19.dt.abbrevs <- data.table(covid19.dt %>% group_by(DINDEX) %>% sample_n(size = 10))
patient_list_abbrev <- unique(covid19.dt.abbrevs[, PATIENT])

tsfile <- "synthea/abbrev.ts"
write_lines("@problemName COVID-19 Survival Abbreviated Dataset", tsfile)
write_lines("@timeStamps false", tsfile, append = TRUE)
write_lines("@missing false", tsfile, append = TRUE)
write_lines("@univariate false", tsfile, append = TRUE)
write_lines(paste("@dimensions", dimens), tsfile, append = TRUE)
write_lines("@equalLength false", tsfile, append = TRUE)
write_lines("@equalLength false", tsfile, append = TRUE)
write_lines(paste("@classLabel true", paste(classVals, collapse = " ")), tsfile, append = TRUE)
write_lines("@data", tsfile, append = TRUE)

for (i in patient_list_abbrev) {
    covid19_tmp <- covid19.dt.abbrevs[PATIENT == i]
    covid19_tmp[, PATIENT := NULL]
    covid19_tmp[, DINDEX := NULL]
    setorderv(covid19_tmp, "TINDEX")
    for (j in covid19_tmp[, TINDEX]) {
        rm(csvData)
        covid19_subtmp <- covid19_tmp[TINDEX <= j]
        obs_count <- nrow(covid19_subtmp)
        covid19_subtmp <- covid19_subtmp[c(1:obs_count, rep(obs_count, max_obs - obs_count))]
        covid19_matrix <- as.matrix(covid19_subtmp)
        debug(logger, paste("Writing ", i, "-", j, " patient data", sep = ""))
        pidx <- paste(i, "-", j, sep = "")
        write.table(t(covid19_matrix),
            file = textConnection("csvData", "w"),
            col.names = FALSE, row.names = FALSE, sep = ",", na = "?"
        )
        tsentry <- paste(csvData, sep = ":", collapse = ":")
        tsentry <- paste(tsentry, covid19.outcomes[PINDEX == pidx, DINDEX], sep = ":")
        write_lines(tsentry, tsfile, append = TRUE)
    }
}


covid19.dt[, PATIENT := NULL]
covid19.dt[, TINDEX := NULL]
covid19.dt.abbrevs[, PATIENT := NULL]
covid19.dt.abbrevs[, TINDEX := NULL]

write_csv(covid19.dt, "synthea/full.csv")
write_csv(covid19.dt.abbrevs, "synthea/abbrevs.csv")