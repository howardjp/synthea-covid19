library(ProjectTemplate)
library("abind")
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
covid19[, OUTCOME := ifelse(is.na(DEATH_TIME), FALSE, TRUE)]
covid19[, DINDEX := time_length(DEATH_TIME - OBSERVATION_TIME, unit = "day")]

covid19.dt <- dcast(data = covid19, formula = PATIENT + TINDEX + DINDEX ~ CODE.x, fun.aggregate = mean, na.rm = TRUE, value.var = "VALUES")

## NAs beat NaNs here
is.nan.data.frame <- function(x) {
    do.call(cbind, lapply(x, is.nan))
}
covid19.dt[is.nan(covid19.dt)] <- NA

covid19.outcomes <- covid19.dt[, list(PATIENT, TINDEX, DINDEX)]
covid19.outcomes[, PINDEX := paste(PATIENT, "-", TINDEX, sep = "")]
info(logger, "Writing mortality data to outputs/outcomes.csv")

patient_list <- unique(covid19.dt[, PATIENT])
patient_list <- sample(patient_list, 10)

mask_maker <- function(x) {
    cumsum(!is.na(x))
}

max_obs <- max(covid19.dt[, .N, by = PATIENT][, N])

info(logger, "Reassemling the matix...and that really sounds cooler than it is")

covid.result <- NULL
covid.matrix <- NULL
covid.matrix.first <- NULL
for (i in patient_list) {
    covid19_tmp <- covid19.dt[PATIENT == i]
    covid19_tmp[, PATIENT := NULL]
    setorderv(covid19_tmp, "TINDEX")
    for (j in covid19_tmp[, TINDEX]) {
        covid19_subtmp <- covid19_tmp[TINDEX <= j]
        duration <- covid19_tmp[TINDEX == j, DINDEX]
        if(is.na(duration)) {
            duration <- covid19_tmp[, max(TINDEX) - min(TINDEX)]
            event <- 0
        } else { event <- 1 }
        obs_count <- nrow(covid19_subtmp)
        covid19_subtmp <- covid19_subtmp[c(1:obs_count, rep(obs_count, max_obs - obs_count))]
        covid19_subtmp[, DINDEX := NULL]
        covid19_subtmp <- as.matrix(covid19_subtmp)
        if(is.null(covid.matrix)) {
            if(is.null(covid.matrix.first)) 
                covid.matrix.first <- covid19_subtmp
            else
                covid.matrix <- abind(covid.matrix.first, covid19_subtmp, along = 0)
        } else {
            covid.matrix <- abind(covid.matrix, covid19_subtmp, along = 1)
        }
        if(is.null(covid.result)) covid.result <- data.frame(duration, event)
        else covid.result <- rbind(covid.result, data.frame(duration, event))

    }
}

covid.result <- as.matrix(covid.result)

info(logger, "Converting to JSON")
covid.matrix.json <- toJSON(covid.matrix, digits = NA, pretty = TRUE, na = "null")
covid.result.json <- toJSON(covid.result, digits = NA, pretty = TRUE, na = "null")

info(logger, "Writing JSON to files")
write(covid.matrix.json, "synthea/abbrev-x.json")
write(covid.result.json, "synthea/abbrev-y.json")

info(logger, "Done.")

# Five different ways of binding together two matrices
x <- matrix(1:12, 3, 4)
y <- x + 100
z <- y + 100
dim(abind(x, y, along = 0)) # binds on new dimension before first
dim(abind(x, y, along = 1)) # binds on first dimension
dim(abind(x, y, along = 1.5))
dim(abind(x, y, along = 2))
dim(abind(x, y, along = 3))
dim(abind(x, y, rev.along = 1)) # binds on last dimension
dim(abind(x, y, rev.along = 0)) # binds on new dimension after last