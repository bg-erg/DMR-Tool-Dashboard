suppressPackageStartupMessages({
  library(shiny, lib.loc = "dependencies")
  library(bslib, lib.loc = "dependencies")
  library(DBI, lib.loc = "dependencies")
  library(RSQLite, lib.loc = "dependencies")
  library(ggplot2, lib.loc = "dependencies")
  library(DT, lib.loc = "dependencies")
  library(tidyverse, lib.loc = "dependencies")
  library(waiter, lib.loc = "dependencies")
  library(openxlsx, lib.loc = "dependencies")
  library(dbplyr, lib.loc = "dependencies")
  library(scales, lib.loc = "dependencies")
  library(tidytext, lib.loc = "dependencies")
  }
)


con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "PLGpar",
  host = "10.3.2.179",
  port = 5432,
  user = "dmr_readonly_user",
  password = "dmr-dashboard-database-access"
)
dbExecute(con, "SET search_path TO 'dmr-database';")


# Initialize functions
source("functions.R")


# Query db

query <- tbl(con, 
             "dmr_2024_nd_as_half")%>%
  filter(facility_type_indicator != "POTW")%>%
  filter(permit_type == "NPD")%>%
  filter(state %in% c("AL", "KY", "MO")) # Limit to main states, to reduce size of full df. We don't need everything

# Get all DMR data 
df_tab <- collect(query)


# New Method --------------------------------------------------------------

# xwalk_psc <- tbl(con, "xwalk_psc_pollcodes")%>%
#   select(!c(parameter_code, parameter_desc))%>%
#   filter(psc_code %in% c("457"))

# xwalk_psc_tab <- xwalk_psc%>%
#   collect()

xwalk_psc <- tbl(con, "xwalk_psc_pollcodes")%>%
  distinct(npdes_permit_number, pollutant_code, psc_code)%>%
  filter(psc_code %in% c("457"))

xwalk_psc_tab <- xwalk_psc%>%
  collect()

expl_dat_new_method <- query%>%
  inner_join(xwalk_psc,
             by = join_by(npdes_permit_number, pollutant_code))%>%
  collect()

expl_dat_new_method


# Old Method --------------------------------------------------------------

xwalk_psc_old <- dbReadTable(con, "xwalk_psc_pollcodes")%>%
  reframe(.by = c(npdes_permit_number, pollutant_code, psc_code))%>%
  tibble()

xwalk_psc_old

expl_dat_old_method <- df_tab%>%
  left_join(xwalk_psc_old,
          by = join_by(npdes_permit_number,
                       pollutant_code),
          relationship = "many-to-many")%>%
  filter(psc_code %in% c("457"))


expl_dat_old_method
expl_dat_new_method



# PSC Code ----------------------------------------------------------------

expl_dat_old_method%>%
  count(psc_code)
expl_dat_new_method%>%
  count(psc_code)


# NPDES ID ----------------------------------------------------------------

expl_dat_old_method%>%
  count(npdes_permit_number)
expl_dat_new_method%>%
  count(npdes_permit_number)



# NODI Code ---------------------------------------------------------------

expl_dat_old_method%>%
  count(nodi_code_description)
expl_dat_new_method%>%
  count(nodi_code_description)




# facility_type_indicator -------------------------------------------------

expl_dat_old_method%>%
  count(facility_type_indicator)
expl_dat_new_method%>%
  count(facility_type_indicator)



# AL0066729 ---------------------------------------------------------------

expl_dat_new_method%>%
  filter(npdes_permit_number == "AL0066729")%>%
  filter(is.na(nodi_code_description) == TRUE)%>%
  filter(monitoring_period_date == ymd("2024-03-31"))%>%
  glimpse()


expl_dat_old_method%>%
  filter(npdes_permit_number == "AL0066729")%>%
  filter(is.na(nodi_code_description) == TRUE)%>%
  filter(monitoring_period_date == ymd("2024-03-31"))%>%
  glimpse()


xwalk_psc_tab%>%
  filter(npdes_permit_number == "AL0066729")%>%
  filter(pollutant_code == "2817")
