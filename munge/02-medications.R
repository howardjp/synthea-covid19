covid19.meds <- merge(medications, encounters, by.x = c("PATIENT", "ENCOUNTER"), by.y = c("PATIENT", "Id"))

info(logger, "Cleaning up data values and types")
covid19.meds[, OBSERVATION_TIME := round_date(as_datetime(START.y), unit = "day")]
covid19.meds[, ENCOUNTERCLASS := as.factor(ENCOUNTERCLASS)]
covid19.meds[, VALUES := as.numeric(DISPENSES)]

info(logger, "Eliminating non-emergency and non-inpatient cases")
covid19.meds <- covid19.meds[ENCOUNTERCLASS == "emergency" | ENCOUNTERCLASS == "inpatient"]
covid19.meds[, ADMIT_TIME := min(OBSERVATION_TIME), by = PATIENT]

info(logger, "Removing all cases with an admission date before March 1, 2020")
covid19.meds <- covid19.meds[ADMIT_TIME >= "2020-03-01"]

covid19.meds[, TINDEX := time_length(OBSERVATION_TIME - ADMIT_TIME, unit = "day")]
covid19.meds.dt <- dcast(data = covid19.meds, formula = PATIENT ~ CODE.x, fun.aggregate = sum, na.rm = FALSE, value.var = "VALUES")
covid19.meds.names <- names(covid19.meds.dt)[-1]          
covid19.meds.names <- paste("MED.", covid19.meds.names, sep = "")
names(covid19.meds.dt) <- c("PATIENT", covid19.meds.names)

cache("covid19.meds.dt")