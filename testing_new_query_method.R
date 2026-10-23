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


xwalk_psc <- tbl(con, "xwalk_psc_pollcodes")%>%
  select(!c(parameter_code, parameter_desc))%>%
  filter(psc_code %in% c("457"))

xwalk_psc_tab <- xwalk_psc%>%
  collect()


# New Method --------------------------------------------------------------


expl_dat_new_method <- query%>%
  inner_join(xwalk_psc,
             by = join_by(npdes_permit_number, pollutant_code))%>%
  collect()



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
