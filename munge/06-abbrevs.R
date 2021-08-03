covid19.dt <- covid19.dt.bk

info(logger, "Starting assembly of abbreviated dataset")
patient_list <- sort(unique(covid19.dt[, PATIENT]))
patient_list <- data.table(PATIENT = sort(sample(patient_list, 30)))
best12 <- names(sort(colSums(sapply(covid19.dt, FUN = is.na)))[1:16])
best12 <- c("DINDEX", best12)
covid19.dt <- covid19.dt[, ..best12]
covid19.dt <- merge(patient_list, covid19.dt, by = "PATIENT", all.x = TRUE)
filename <- "output/abbrev.json"

make_json(covid19.dt, patient_list[, PATIENT], filename)

exit()