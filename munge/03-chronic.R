chronic.dt <- conditions[STOP == ""]
chronic.dt <- chronic.dt[!grep("(finding)", DESCRIPTION)]
covid19.chrn.dt <- dcast(data = chronic.dt, PATIENT ~ CODE, length)
covid19.chrn.names <- names(covid19.chrn.dt)[-1]
covid19.chrn.names <- paste("CHRN.", covid19.chrn.names, sep = "")
names(covid19.chrn.dt) <- c("PATIENT", covid19.chrn.names)

cache("covid19.chrn.dt")