library(DBI)
library(RSQLite)
library(tidyverse)
library(janitor)
library(dbplyr)


push_api_dat(db_loc = "inputs/dmr-database.db",
             api_dat_parent_folder = "C:/Users/BGrubbs/OneDrive - Eastern Research Group/Project Documents/ELG Planning/DMR Cleaning for OWA/epa_api_dl/",
             api_dat_subfolder_loc = "nd_half",
             nd_handling_method = "Half")
