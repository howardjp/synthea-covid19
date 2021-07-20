covid19.ptxs <- patients[, list(Id, AGE = time_length(as_datetime("2020-03-01") - as_datetime(BIRTHDATE), unit = "year"), GENDER = as.factor(GENDER), RACE = as.factor(RACE), ETHNICITY = as.factor(ETHNICITY), MARITAL = as.factor(MARITAL))]
covid19.ptxs[MARITAL == "", MARITAL := NA]
covid19.ptxs.marital <- covid19.ptxs[, list(Id, MARITAL = as.factor(as.character(MARITAL)))]
covid19.ptxs.dt <- dcast(data = covid19.ptxs,  Id + AGE + GENDER + ETHNICITY ~ RACE, length)
covid19.ptxs.marital.dt <- dcast(data = covid19.ptxs.marital, Id ~ MARITAL, length)
covid19.ptxs.marital.dt[, "NA" := NULL]
covid19.ptxs.dt <- merge(covid19.ptxs.dt, covid19.ptxs.marital.dt, by = "Id")
covid19.ptxs.dt[, GENDER := as.integer(GENDER) - 1]
covid19.ptxs.dt[, ETHNICITY := as.integer(ETHNICITY) - 1]

cache("covid19.ptxs.dt")