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


db_path <- "inputs/dmr-database.db"
con <- dbConnect(RSQLite::SQLite(), db_path)

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
psc_desc <- dbReadTable(con, "psc_list")%>% 
  reframe(.by = c(psc_code, psc_desc))