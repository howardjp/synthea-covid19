info(logger, "Starting assembly of full dataset")
filename <- "output/full.json"
covid19.dt <- covid19.dt.bk
make_json(covid19.dt, filename)
