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
outcomes[, DEATH_TIME := as_datetime(DEATHDATE)]
covid19 <- merge(covid19, outcomes, by.x = "PATIENT", by.y = "Id", all.x = TRUE)
covid19[, OUTCOME := ifelse(is.na(DEATH_TIME), 0, 1)]
covid19[, DINDEX := time_length(DEATH_TIME - OBSERVATION_TIME, unit = "day")]

covid19.obvs.dt <- dcast(data = covid19, formula = PATIENT + TINDEX + DINDEX + OUTCOME ~ CODE.x, fun.aggregate = mean, na.rm = TRUE, value.var = "VALUES")
cache("covid19.obvs.dt")