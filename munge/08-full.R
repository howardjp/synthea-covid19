info(logger, "Starting assembly of full dataset")
filename <- "output/full.json"
patient_list <- sort(unique(covid19.dt[, PATIENT]))
covid19.dt <- covid19.dt.bk
make_json(covid19.dt, patient_list, filename)
