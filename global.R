min_year <- 2015
heute <- Sys.Date()
heute_monatsanfang <- as.Date(paste0(substr(as.character(heute), 1, 8), "01"))
today_doy <- as.numeric(format(heute, format = "%j"))
today_moy <- as.numeric(format(heute, format = "%m"))
today_year <- as.numeric(format(heute, format = "%Y"))
