library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(ggplot2)
library(DT)
library(stringr)
library(tidyverse)
library(readxl)
library(janitor)
library(data.table)

# Identify Data Locations ----------------------------
# DMR Database
db_path <- "inputs/dmr-database.db"

# Connect to Database -----------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), db_path)

dbListTables(con)

#dbListFields(con, "DMR_2024_NDasZero")


# Create Table: PSC Code to Description Crosswalk ---------------------------------
# psc_loc <- "inputs/crosswalks/SIC-PSC-Standard-Crosswalk.csv"
# 
# xwalk_sic_psc <- read_csv(psc_loc)
# 
# list_pscs <- xwalk_sic_psc%>%
#   distinct(PSC_CODE, PSC_DESC)%>%
#   arrange(PSC_CODE)%>%
#   rename("psc_code" = "PSC_CODE", 
#          "psc_desc" = "PSC_DESC")%>%
#   mutate(psc_code = as.character(psc_code))%>%
#   add_row(psc_code = "503", 
#           psc_desc = "Miscellaneous foods and beverages")%>%
#   add_row(psc_code = "505", 
#           psc_desc = "Food service establishments")%>%
#   add_row(psc_code = "507", 
#           psc_desc = "Independent and stand alone labs")%>%
#   add_row(psc_code = "600", 
#           psc_desc = "Unassigned waste facilities")
#   
# 
# psc_desc <- list_pscs%>%
#   mutate(psc_full = paste0(psc_code, " - ", psc_desc))
# 
# DBI::dbRemoveTable(conn = con,
#                    name = "psc_list")
# 
# DBI::dbWriteTable(conn = con,
#                   name = "psc_list",
#                   value = psc_desc,
#                   overwrite = TRUE)


# Create Table: States Available for Selection ----------------------------

#state_choices <- dbGetQuery(con, "SELECT DISTINCT STATE_CODE FROM 'LTMonPerCustomSearch-2024Non-POTWs-08142026' ORDER BY STATE_CODE")

# DBI::dbWriteTable(conn = con,
#                   name = "states_list",
#                   value = state_choices,
#                   overwrite = TRUE)


# Create Table: Pollutants Available for Selection ------------------------

# pollutant_list <- dbGetQuery(con, "SELECT DISTINCT POLLUTANT_DESC FROM 'LTMonPerCustomSearch-2024Non-POTWs-08142026' ORDER BY POLLUTANT_DESC")

# DBI::dbWriteTable(conn = con,
#                   name = "pollutant_list",
#                   value = pollutant_list,
#                   overwrite = TRUE)


# Create Table: SIC Codes Available for Selection ------------------------

# sic_list <- dbGetQuery(con, "SELECT DISTINCT SICCODE FROM 'LTMonPerCustomSearch-2024Non-POTWs-08142026' ORDER BY SICCODE")

# DBI::dbWriteTable(conn = con,
#                   name = "sic_list",
#                   value = sic_list,
#                   overwrite = TRUE)



# Create Table: SIC to PSC Code Crosswalk ------------------------

# xwalk_psc_pollcodes <- read_csv("inputs/crosswalks/304M-PSC-XWALK-POLLCODES-08-14-2026.csv",
#                                 col_types = "ccccc")
# xwalk_psc_pollcodes
# 
# 
# DBI::dbWriteTable(conn = con,
#                   name = "xwalk_psc_pollcodes",
#                   value = xwalk_psc_pollcodes,
#                   overwrite = TRUE)



# Create Table: 2024 Annual Review Filter List ----------------------------
# Dataframe that identifies facilities used in 2024 annual review ---------

# farah_join_path <- "K:/PROJECTS2-FFX/304m/DMR2024/FA files/Join_DMR2024.csv"

# filt_list_farah <- read_csv(farah_join_path,
#                             col_types = cols(.default = col_character()))%>%
#   filter(LBY_304M != 0)%>%
#   reframe(.by = c(EXTERNAL_PERMIT_NMBR, PSC_CODE_304_M))


# DBI::dbWriteTable(conn = con,
#                   name = "filt_list_farah",
#                   value = filt_list_farah,
#                   overwrite = TRUE)



# Create Table: 2024 DMR Data with NDs Replaced with 1/2 MDL  -------------


# push_api_dat(db_loc = "inputs/dmr-database.db",
#              api_dat_parent_folder = "C:/Users/BGrubbs/OneDrive - Eastern Research Group/Project Documents/ELG Planning/DMR Cleaning for OWA/epa_api_dl/",
#              api_dat_subfolder_loc = "nd_zero",
#              nd_handling_method = "Zero")


# 
# # Correcting Field Names --------------------------------------------------
# 
# dbListTables(con)
# dbListFields(con, "DMR_2024_NDasHalf")
# 
# all_field_names <- dbListFields(con, "DMR_2024_NDasZero")
# all_field_names
# 
# ################################################################################
# dbListFields(con, "xwalk_psc_pollcodes")
# dbExecute(con, "ALTER TABLE xwalk_psc_pollcodes RENAME COLUMN EXTERNAL_PERMIT_NMBR TO npdes_permit_number;")
# dbExecute(con, "ALTER TABLE xwalk_psc_pollcodes RENAME COLUMN POLLUTANT_CODE TO pollutant_code;")
# dbExecute(con, "ALTER TABLE xwalk_psc_pollcodes RENAME COLUMN PSC_CODE_304_M TO psc_code;")
# 
# dbListFields(con, "xwalk_psc_pollcodes")
# 
# ################################################################################
# 
# dbListFields(con, "sic_list")
# dbExecute(con, "ALTER TABLE sic_list RENAME COLUMN SICCODE TO sic_code;")
# dbListFields(con, "sic_list")
# 
# ################################################################################
# 
# dbListFields(con, "states_list")
# dbExecute(con, "ALTER TABLE states_list RENAME COLUMN STATE_CODE TO state;")
# dbListFields(con, "states_list")
# 
# ################################################################################
# 
# dbListFields(con, "pollutant_list")
# dbExecute(con, "ALTER TABLE pollutant_list RENAME COLUMN POLLUTANT_DESC TO pollutant_description;")
# dbListFields(con, "pollutant_list")
# 
# ################################################################################
# 
# dbListFields(con, "filt_list_farah")
# dbExecute(con, "ALTER TABLE filt_list_farah RENAME COLUMN EXTERNAL_PERMIT_NMBR TO npdes_permit_number;")
# dbExecute(con, "ALTER TABLE filt_list_farah RENAME COLUMN PSC_CODE_304_M TO psc_code;")
# dbListFields(con, "filt_list_farah")
# # 
# 
# 
# 
# # Upload DMR Data pulled by Eva -------------------------------------------
# dmr_dat_eva <- fread("inputs/reference_dat/LTMonPerCustomSearch-2024Non-POTWs-08142026.csv")
# 
# 
# DBI::dbWriteTable(conn = con,
#                   name = "dmr_dat_eva",
#                   value = dmr_dat_eva,
#                   overwrite = TRUE)
