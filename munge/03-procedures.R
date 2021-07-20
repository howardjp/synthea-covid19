covid19.prcs <- merge(procedures, encounters, by.x = c("PATIENT", "ENCOUNTER"), by.y = c("PATIENT", "Id"))
info(logger, "Cleaning up data values and types")
covid19.prcs[, OBSERVATION_TIME := round_date(as_datetime(START), unit = "day")]
covid19.prcs[, ENCOUNTERCLASS := as.factor(ENCOUNTERCLASS)]
covid19.prcs[, VALUES := 1]

info(logger, "Eliminating non-emergency and non-inpatient cases")
covid19.prcs <- covid19.prcs[ENCOUNTERCLASS == "emergency" | ENCOUNTERCLASS == "inpatient"]
covid19.prcs[, ADMIT_TIME := min(OBSERVATION_TIME), by = PATIENT]

info(logger, "Removing all cases with an admission date before March 1, 2020")
covid19.prcs <- covid19.prcs[ADMIT_TIME >= "2020-03-01"]

covid19.prcs[, TINDEX := time_length(OBSERVATION_TIME - ADMIT_TIME, unit = "day")]
covid19.prcs.dt <- dcast(data = covid19.prcs, formula = PATIENT ~ CODE.x, fun.aggregate = sum, value.var = "VALUES")
covid19.prcs.names <- names(covid19.prcs.dt)[-1]          
covid19.prcs.names <- paste("PRCS.", covid19.prcs.names, sep = "")
names(covid19.prcs.dt) <- c("PATIENT", covid19.prcs.names)

cache("covid19.prcs.dt")