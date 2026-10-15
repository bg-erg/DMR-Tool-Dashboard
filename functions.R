

# Get Options Available for User to Select on UI --------------------------

fn_get_input_choices <- function(db_path = "inputs/dmr-database.db",
                                 db_dbname = "PLGpar",
                                 db_host = "10.3.2.179",
                                 db_port = 5432,
                                 db_user = "dmr_readonly_user",
                                 db_password = "dmr-dashboard-database-access"){
  con <- dbConnect(
    RPostgres::Postgres(),
    dbname = db_dbname,
    host = db_host,
    port = db_port,
    user = db_user,
    password = db_password
  )
  dbExecute(con, "SET search_path TO 'dmr-database';")


  sic_choices <- dbReadTable(con, "xwalk_psc_code_to_desc")$sic_code
  sic_choices <- sic_choices[!is.na(sic_choices)]
  
  pollutant_choices <- dbReadTable(con, "pollutant_list")$pollutant_description
  pollutant_choices <- pollutant_choices[!is.na(pollutant_choices)]
  
  state_choices <- dbReadTable(con, "states_list")$state
  state_choices <- state_choices[!is.na(state_choices)]
  
  psc_choices <- dbReadTable(con, "xwalk_psc_code_to_desc")$psc_full

  return(list(state_choices = state_choices, 
              pollutant_choices = pollutant_choices, 
              sic_choices = sic_choices,
              psc_choices = psc_choices))
}


# Load Subset of DMR Data and Prepare for Summarization -------------------

fn_process_dmr_dat <- function(filtered_dmr_df, 
                               retained_nodi_codes = c("B - Below Detection Limit/No Detection")){
  
  # 1. Read in the DMR data 
  # 2. Join with the crosswalk from NPDES ID and Pollutant Code to PSC, also provided by Eva
  # 3. Join with data used by Farah for previous analyses; discrepancies reviewed in 
  #    '01_Identify_Possible_Crosswalk_Issues.R' script. 
  #    NOTE: The join is many-to-many because a single facility can have multiple PSC codes, and
  #           there are multiple entries for each npdes_permit_number + Pollutant Code combination
  # in the source dataset. 
  # 4. Filter out facility/pollutant combinations that are missing a PSC code
  
  # We exclude all records where the facility reported a NODI code with the exception 
  # of the following codes
  nodi_code_retain <- retained_nodi_codes
  
  ### Crosswalk from Permit No. and Pollutant Code to PSC Code ----------------
  xwalk_psc <- dbReadTable(con, "xwalk_psc_pollcodes")%>%
    reframe(.by = c(npdes_permit_number, 
                    pollutant_code,
                    psc_code))%>%
    tibble()
  
  ### Crosswalk to Identify Plants Used in Annual Rankings --------------------
  # ALREADY FILTERED BY REQUIRED CRITERIA
  # If ALL of the samples at a facility have ZERO loads, then exclude that facility.
  # So if there's a SINGLE pollutant at the facility with a loading, it's included
  annual_review_facs_2024 <- dbReadTable(con, "annual_review_facs_2024")
  
  ## Read in DMR Data --------------------------------------------------------
  
  # Read in the DMR data from EPA Loading Tool
  # Join with data used by Farah for previous analyses; discrepancies reviewed in 
  #    '01_Identify_Possible_Crosswalk_Issues.R' script. 
  # Filter out facility/pollutant combinations that are missing a PSC code
  
  dmr_dat <- filtered_dmr_df%>%
    filter(facility_type_indicator != "POTW")%>% # Exclude POTWs
    mutate(pollutant_code = as.character(pollutant_code))%>%
    mutate(across(where(is_character), ~na_if(., "")))%>%
    tibble()%>%
    inner_join(annual_review_facs_2024, 
               by = join_by(npdes_permit_number, 
                            psc_code))%>%
    # Filter out rows where no PSC Code is available/matched. Large percentage is expected and ok
    filter(is.na(psc_code) == FALSE)
  
  
  ## Revise DMR Data for Summarization ---------------------------------------
  
  # 1. Only include records where there's no NODI Code or the NODI Code is one of 
  #    the exceptions that we decided to retain
  
  # 2. If a record has non-detect NODI Code AND the average_concentration_mg_l value (calculated by the loading tool)
  #   is NA (blank), then update the concentration to be ZERO. This prevents those records from being ignored
  #   in summary statistics
  #   OR
  #   If includes_non_detects == "Yes" and there's no conc value
  #   Then 
  #   Set to zero
  
  dmr_dat_mod <- dmr_dat%>%
    # Step 1 - Exclude NODI Codes other than B 
    filter(is.na(nodi_code_description) == TRUE | nodi_code_description %in% nodi_code_retain)%>%
    # Step 2 - Set avg. conc to 0 according to condiions
    mutate(average_concentration_mg_l_mod = case_when(
      nodi_code_description == "B - Below Detection Limit/No Detection" & is.na(average_concentration_mg_l) == TRUE ~ 0,
      includes_non_detects == "Yes" & is.na(average_concentration_mg_l) == TRUE ~ 0, 
      .default = average_concentration_mg_l))
  
  return(dmr_dat_mod)
}


# Calculate Summary Statistics for Processed DMR Data by PSC Code---------------------

fn_get_summary_stats <- function(dmr_dat_mod){
  
  ## Summarize by PSC and Pollutant ------------------------------------------
  # To account for overrepresentation of some facilities, we average by date then 
  # monitoring location then facility, then PSC,
  
  
  dmr_summary <- dmr_dat_mod%>%
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = n(),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number,
                      monitoring_location_code,
                      monitoring_period_date))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by PSC, Pollutant, Facility, and Monitoring Point
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = sum(record_count),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number,
                      monitoring_location_code))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by PSC, Pollutant, and Facility
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = sum(record_count),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by PSC and Pollutant
    summarize(record_count = sum(record_count),
              facility_count = n_distinct(npdes_permit_number),
              mean = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              median = median(average_concentration_mg_l_mod, na.rm = TRUE),
              sd = sd(average_concentration_mg_l_mod, na.rm = TRUE),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description))
  
  
  # Calculate number of facilities that reported data with average_concentration_mg_l *greater than zero*
  # That "greater than" bit is important, since it means that replacing NAs with 0 doesn't impact this calculation
  # This excludes facilities that reported data that was always NA
  facs_over_zero <- dmr_dat_mod%>%
    filter(!is.na(average_concentration_mg_l_mod) & average_concentration_mg_l_mod > 0)%>%
    summarize(facility_count_non_na_greater_zero = n_distinct(npdes_permit_number),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description))
  
  ## Calculate Min and Max of ALL data --------------------------------------
  # Calculate based on complete dataset to show the full range of reported average values.
  
  dmr_min_max <- dmr_dat_mod%>%
    summarize(min = min(average_concentration_mg_l_mod, na.rm = TRUE),
              max = max(average_concentration_mg_l_mod, na.rm = TRUE),
              .by = c(psc_code,
                      pollutant_code,
                      pollutant_description))
  

  # Append the count of facilities that reported any samples over 0 mg/L, 
  # select desired fields and clean their names
  dmr_summary_clean <- dmr_summary%>%
    left_join(facs_over_zero,
              by = join_by(psc_code, pollutant_code, pollutant_description))%>%
    left_join(dmr_min_max,
              by = join_by(psc_code, pollutant_code, pollutant_description))%>%
    left_join(psc_desc,
              by = join_by(psc_code == psc_code))%>%
    mutate(facility_count_non_na_greater_zero = ifelse(is.na(facility_count_non_na_greater_zero),
                                                       0,
                                                       facility_count_non_na_greater_zero))%>%
    select('PSC Code' = psc_code,
           'PSC Description' = psc_desc,
           'Pollutant Code' = pollutant_code, 
           'Pollutant Description' = pollutant_description,
           'Min (mg/L)' = min,
           'Max (mg/L)' = max,
           'Mean (mg/L)' = mean,
           'Median (mg/L)' = median,
           'SD (mg/L)' = sd,
           'Number of Facilities Reporting Data' = facility_count,
           'Number of Facilities Reporting > 0' = facility_count_non_na_greater_zero, 
           'Count of Records' = record_count)

  return(dmr_summary_clean)
  
}

# Calculate Summary Statistics for Processed DMR Data by SIC Code---------------------

fn_get_sic_summary_stats <- function(dmr_dat_mod){
  
  ## Summarize by SIC and Pollutant ------------------------------------------
  # To account for overrepresentation of some facilities, we average by date then 
  # monitoring location then facility, then SIC,
  
  
  dmr_summary <- dmr_dat_mod%>%
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = n(),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number,
                      monitoring_location_code,
                      monitoring_period_date))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by SIC, Pollutant, Facility, and Monitoring Point
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = sum(record_count),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number,
                      monitoring_location_code))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by SIC, Pollutant, and Facility
    summarize(average_concentration_mg_l_mod = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              record_count = sum(record_count),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description,
                      npdes_permit_number))%>%
    mutate(average_concentration_mg_l_mod = na_if(average_concentration_mg_l_mod, NaN))%>%
    # Average by SIC and Pollutant
    summarize(record_count = sum(record_count),
              facility_count = n_distinct(npdes_permit_number),
              mean = mean(average_concentration_mg_l_mod, na.rm = TRUE),
              median = median(average_concentration_mg_l_mod, na.rm = TRUE),
              sd = sd(average_concentration_mg_l_mod, na.rm = TRUE),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description))
  
  
  # Calculate number of facilities that reported data with average_concentration_mg_l *greater than zero*
  # That "greater than" bit is important, since it means that replacing NAs with 0 doesn't impact this calculation
  # This excludes facilities that reported data that was always NA
  facs_over_zero <- dmr_dat_mod%>%
    filter(!is.na(average_concentration_mg_l_mod) & average_concentration_mg_l_mod > 0)%>%
    summarize(facility_count_non_na_greater_zero = n_distinct(npdes_permit_number),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description))
  
  ## Calculate Min and Max of ALL data --------------------------------------
  # Calculate based on complete dataset to show the full range of reported average values.
  
  dmr_min_max <- dmr_dat_mod%>%
    summarize(min = min(average_concentration_mg_l_mod, na.rm = TRUE),
              max = max(average_concentration_mg_l_mod, na.rm = TRUE),
              .by = c(sic_code,
                      pollutant_code,
                      pollutant_description))
  
  
  # Append the count of facilities that reported any samples over 0 mg/L, 
  # select desired fields and clean their names
  dmr_summary_clean <- dmr_summary%>%
    left_join(facs_over_zero,
              by = join_by(sic_code, pollutant_code, pollutant_description))%>%
    left_join(dmr_min_max,
              by = join_by(sic_code, pollutant_code, pollutant_description))%>%
    mutate(facility_count_non_na_greater_zero = ifelse(is.na(facility_count_non_na_greater_zero),
                                                       0,
                                                       facility_count_non_na_greater_zero))%>%
    select('SIC Code' = sic_code,
           'Pollutant Code' = pollutant_code, 
           'Pollutant Description' = pollutant_description,
           'Min (mg/L)' = min,
           'Max (mg/L)' = max,
           'Mean (mg/L)' = mean,
           'Median (mg/L)' = median,
           'SD (mg/L)' = sd,
           'Number of Facilities Reporting Data' = facility_count,
           'Number of Facilities Reporting > 0' = facility_count_non_na_greater_zero, 
           'Count of Records' = record_count)
  
  return(dmr_summary_clean)
  
}




# File Export Functions ---------------------------------------------------





# Filename for Summary Data -----------------------------------

fn_summ_dat_filename <- function(file_extension = ".xlsx"){
  export_file_name <- paste0("DMR_",
                             selected_year,
                             "_Summary_Data_",
                             format(Sys.Date(), "%Y.%m.%d"),
                             file_extension)
  
  return(export_file_name)
}


# Filename for Full Dataset -----------------------------------

fn_full_dat_filename <- function(file_extension = ".csv"){
  export_file_name <- paste0("DMR_", 
                             selected_year,
                             "_Full_Data_",
                             format(Sys.Date(), "%Y.%m.%d"),
                             file_extension)
  
  return(export_file_name)
}
















# Visualizations ----------------------------------------------------------
## Top 10 pollutants in the selected PSC Code(s) ------------------------------


get_n_pscs <- function(summarized_dat){
  
  n_pscs <- length(unique(summarized_dat$`PSC Description`))
  return(n_pscs)
  
}

summarize_top_polls <- function(summarized_dat){
  
  summarized_dat%>%
    slice_max(`Mean (mg/L)`,
            n = 10,
            by = `PSC Description`)%>%
    ggplot(aes(x = tidytext::reorder_within(x = `Pollutant Description`, 
                                            by = -`Mean (mg/L)`,
                                            within = `PSC Description`),
               y = `Mean (mg/L)`))+
    facet_wrap(facets = vars(`PSC Description`),
               scales = "free_x",
               ncol = 2)+
    geom_bar(stat = "identity",
             fill = "steelblue")+
    scale_x_discrete(labels = ~ label_wrap(40)(str_remove(.x, "___.+"))) +
    scale_y_continuous(labels = label_comma())+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    labs(
      x = "Pollutant Descriptiom",
      y = "",
      title = "Mean Concentration (mg/L) of the Top 10 Pollutants per Point Source Category")

}






# `--------------------------------------------------------------------
# `--------------------------------------------------------------------

# Push API Data Obtained from Loading Tool to Database --------------------

push_api_dat <- function(db_loc = "inputs/dmr-database.db",
                         api_dat_parent_folder = "C:/Users/BGrubbs/OneDrive - Eastern Research Group/Project Documents/ELG Planning/DMR Cleaning for OWA/epa_api_dl/",
                         api_dat_subfolder_loc = "nd_half",
                         nd_handling_method = "Half"){
  
  # Define Classifications of Fields ------------
  
  field_class_vals <- cols(
    `Monitoring Period Date` = col_character(),
    `NPDES Permit Number` = col_character(),
    `Facility Name` = col_character(),
    `Facility Type Indicator` = col_character(),
    `Permit Type` = col_character(),
    `Permit Effective Date` = col_character(),
    `Permit Expiration Date` = col_character(),
    `Major/Non-Major Status` = col_character(),
    `SIC Code` = col_character(),
    `Street Address` = col_character(),
    City = col_character(),
    State = col_character(),
    `ZIP Code` = col_character(),
    County = col_character(),
    `EPA Region` = col_character(),
    `Congressional District` = col_character(),
    `HUC 12 Code` = col_character(),
    `Watershed Name` = col_character(),
    `Water Body Name` = col_character(),
    `Reach Code` = col_character(),
    `Listed for Impairment` = col_character(),
    `Outfall Number` = col_character(),
    `Limit Set Designator` = col_character(),
    `Pollutant Description` = col_character(),
    `Measurement Quantity 1 Qualifier` = col_character(),
    `Measurement Quantity 2 Qualifier` = col_character(),
    `Measurement Concentration 1 Qualifier` = col_character(),
    `Measurement Concentration 2 Qualifier` = col_character(),
    `Measurement Concentration 3 Qualifier` = col_character(),
    `Includes Non-Detects?` = col_character(),
    `Measurement Type` = col_character(),
    `NODI Code/Description` = col_character(),
    `FRS Tribal Land Code` = col_character(),
    `ICIS Tribal Land Flag` = col_character(),
    `Within Spatial Tribal Boundary` = col_character(),
    `Nearby Tribes` = col_character(),
    `Permit Status Code` = col_character(),
    `TRI Facility ID(s)` = col_character(),
    `CWNS ID(s)` = col_character(),
    `Impairment Class` = col_character(),
    `Parameter Code` = col_character(),
    `Parameter Description` = col_character(),
    `Potential Outlier?` = col_character(),
    `FRS ID` = col_character(),
    `NAICS Code` = col_character(),
    `Number of Report` = col_character(),
    `Pollutant Code` = col_character(),
    `CAS Number` = col_character(),
    `Substance Registry System ID` = col_character(),
    `Monitoring Location Code` = col_character(),
    
    
    `Number of Active CSO Outfalls` = col_double(),
    `Facility Latitude` = col_double(),
    `Facility Longitude` = col_double(),
    `Total Facility Design Flow (MGD)` = col_double(),
    `Actual Average Facility Flow (MGD)` = col_double(),
    `Permit Feature Latitude` = col_double(),
    `Permit Feature Longitude` = col_double(),
    `Season Number` = col_double(),
    `Toxic Weighting Factor (TWF)` = col_double(),
    `Limit Quantity 1 (Avg, kg/day)` = col_double(),
    `Limit Quantity 2 (Max, kg/day)` = col_double(),
    `Limit Concentration 1 (Min, mg/L)` = col_double(),
    `Limit Concentration 2 (Avg, mg/L)` = col_double(),
    `Limit Concentration 3 (Max, mg/L)` = col_double(),
    `Measurement Quantity 1 (Avg, kg/day)` = col_double(),
    `Measurement Quantity 2 (Max, kg/day)` = col_double(),
    `Measurement Concentration 1 (Min, mg/L)` = col_double(),
    `Measurement Concentration 2 (Avg, mg/L)` = col_double(),
    `Measurement Concentration 3 (Max, mg/L)` = col_double(),
    `Pollutant Load (kg/period)` = col_double(),
    `Average Daily Load (kg/day)` = col_double(),
    `Average Concentration (mg/L)` = col_double(),
    `% Load from DMRs` = col_double(),
    `% Load from Limits` = col_double(),
    `Max Allowable Load (kg/period)` = col_double(),
    `Load Over Limit (Option 1) (kg/period)` = col_double(),
    `Load Over Limit (Option 2) (kg/period)` = col_double(),
    `Wastewater Flow (MGal/period)` = col_double(),
    `Maximum Daily Flow (MGD)` = col_double(),
    `Average Wastewater Temp (deg F)` = col_double(),
    `Average Wastewater pH` = col_double(),
    `Percent People of Color (3 mi)` = col_double(),
    `Percent Low Income (3 mi)` = col_double(),
    `Population Density` = col_double())
  
  # Get List of API Data -----------------------------------------------------------
  api_dat_loc <- paste0(api_dat_parent_folder, api_dat_subfolder_loc)
  
  file_list<-list.files(path = api_dat_loc, 
                        full.names = TRUE,
                        pattern = "\\.csv$")
  
  con <- dbConnect(RSQLite::SQLite(), db_loc)
  
  field_names <- read_csv(file_list[1],
                          skip = 3)%>%
    clean_names()%>%
    slice(0)
  
  table_name <- paste0("DMR_2024_NDas",
                       nd_handling_method)
  
  
  if(any(grepl(table_name, dbListTables(con))) == TRUE){
    
    dbRemoveTable(conn = con,
                  name = table_name)
  } 
  
  dbCreateTable(conn = con,
                name = table_name,
                fields = field_names)
  
  i = 1
  assign("last.warning", NULL, envir = baseenv())
  
  
  for(i in 1:length(file_list)){
    
    loc <- file_list[i]

    print(paste0("Appending ", str_extract(file_list[i], "api_[A-Z]{2}_\\d{4}-\\d{2}_\\d{4}-\\d{2}_\\w+")))
    
    dmr_dat_tab <- read_csv(loc, 
                            skip = 3,
                            col_types = field_class_vals)%>%
      clean_names()
    
    print(problems(dmr_dat_tab))
    print(warnings())
    
    assign("last.warning", NULL, envir = baseenv())
    
    DBI::dbAppendTable(conn = con,
                       name = table_name,
                       value = dmr_dat_tab)
    
    print(warnings())
    assign("last.warning", NULL, envir = baseenv())
    
    print(paste0("Successfully Appended: ", file_list[i]))
    
  }
  
}

