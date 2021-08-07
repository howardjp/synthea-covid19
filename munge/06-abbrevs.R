info(logger, "Starting assembly of abbreviated dataset")

covid19.dt.core_columns <- c("PATIENT", "DINDEX", "TINDEX", "OUTCOME")
covid19.dt.ig <- information.gain(OUTCOME ~ ., data = covid19.dt)
covid19.dt.ig.dt <- data.table(attr = row.names(covid19.dt.ig), covid19.dt.ig)
covid19.dt.ig.dt <- covid19.dt.ig.dt[!(attr %in% covid19.dt.core_columns),]
setorder(covid19.dt.ig.dt, -attr_importance)

patient_counts <- seq(30, 100, 10)
feature_counts <- seq(12, 144, 12)

covid19.dt <- covid19.dt.bk
main_patient_list <- sort(unique(covid19.dt[, PATIENT]))

for(i in patient_counts) {
    info(logger, paste("Starting assembly for patient count", i))
    patient_list <- data.table(PATIENT = sort(sample(main_patient_list, i)))
    for (j in feature_counts) {
        info(logger, paste("Starting assembly for patient count", i, "feature count", j))
        covid19.dt <- covid19.dt.bk
        best_cols <- head(covid19.dt.ig.dt, n = j)$attr 
        best_cols <- c(covid19.dt.core_columns, best_cols)
        covid19.dt.abbrev <- covid19.dt[, ..best_cols]
        covid19.dt <- merge(patient_list, covid19.dt, by = "PATIENT", all.x = TRUE)
        filename <- paste("output/abbrev-", i, "-", j, ".json", sep = "")
        make_json(covid19.dt, patient_list[, PATIENT], filename)
    }
}
