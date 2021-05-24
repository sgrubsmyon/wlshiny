heute <- Sys.Date()
heute_monatsanfang <- as.Date(paste0(substr(as.character(heute), 1, 8), "01"))
today_doy <- as.numeric(format(heute, format = "%j"))
today_year <- as.numeric(format(heute, format = "%Y"))
