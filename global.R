# Eventually make it so required packages are all saved in a local subdirectory
# Being buggy rn
# suppressPackageStartupMessages({
#   library(shiny, lib.loc = "dependencies")
#   library(bslib, lib.loc = "dependencies")
#   library(DBI, lib.loc = "dependencies")
#   library(RSQLite, lib.loc = "dependencies")
#   library(ggplot2, lib.loc = "dependencies")
#   library(DT, lib.loc = "dependencies")
#   library(tidyverse, lib.loc = "dependencies")
#   library(waiter, lib.loc = "dependencies")
#   library(openxlsx, lib.loc = "dependencies")
#   library(dbplyr, lib.loc = "dependencies")

# })

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DBI)
  library(RSQLite)
  library(ggplot2)
  library(DT)
  library(tidyverse)
  library(waiter)
  library(openxlsx)
  library(dbplyr)
  library(tidytext)
  library(scales)
})

options(shiny.reactlog=TRUE) 

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

# Get user choices from database
user_choices <- fn_get_input_choices()

# Define Waiter Settings --------------------------------------------------------
query_loading_screen <- tagList(
  spin_flower(), # Preview other spinners by running `preview_spinner(spin_chasing_dots())`
  h3("Querying Database"),
  h5("Please be patient, depending on the size of the resulting table, this can take a while!"))


# Get crosswalk from PSC Code to PSC Description from database-------------------
# Add four additional PSC descriptions that aren't official ELGs; used internally
psc_desc <- dbReadTable(con, "xwalk_psc_code_to_desc")%>% 
  reframe(.by = c(psc_code, psc_desc))
