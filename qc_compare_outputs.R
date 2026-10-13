library(readxl)


filt_list_farah <- dbReadTable(con, "filt_list_farah")

xwalk_psc <- dbReadTable(con, "xwalk_psc_pollcodes")%>%
  reframe(.by = c(npdes_permit_number, 
                  pollutant_code,
                  psc_code))%>%
  tibble()%>%
  mutate(pollutant_code = as.numeric(pollutant_code))


# Function that checks if records are missing ------------------------
# Runs anti-joins to see if any records are missing
# Note: Floating point errors can cause some matches to fail (i.e., join thinking that 0.0005543244 != 0.0005543244)
# This is accounted for by rounding to the 10th decimal point with round(avg_conc, 10)

compare_datasets <- function(dashboard_df, 
                             owa_df){
  
  comp1 <- dashboard_df%>%
    mutate(pollutant_code = as.character(pollutant_code))%>% # Convert for compatability
    mutate(average_concentration_mg_l_mod = round(average_concentration_mg_l_mod, 10))%>% 
    anti_join(owa_df%>%
                mutate(average_concentration_mg_l_mod = round(average_concentration_mg_l_mod, 10)),
              by = join_by(monitoring_period_date, 
                           npdes_permit_number, 
                           pollutant_code, 
                           monitoring_location_code, 
                           average_concentration_mg_l_mod))
  
  comp2 <- owa_df%>%
    mutate(average_concentration_mg_l_mod = round(average_concentration_mg_l_mod, 10))%>% 
    anti_join(dashboard_df%>%
                mutate(average_concentration_mg_l_mod = round(average_concentration_mg_l_mod, 10))%>%
                mutate(pollutant_code = as.character(pollutant_code)),
              by = join_by(monitoring_period_date, 
                           npdes_permit_number, 
                           pollutant_code, 
                           monitoring_location_code, 
                           average_concentration_mg_l_mod))
  
  if(nrow(comp1) == 0 & nrow(comp2) == 0){
    result = "No Unmatched Rows!"
  } else if(nrow(comp1) != 0 & nrow(comp2) == 0){
    result = "Unmatched Rows in Dataset from Dashboard"
    
  } else if(nrow(comp1) == 0 & nrow(comp2) != 0){
    result = "Unmatched Rows in Dataset from OWA Summarization Script"
  } else {
    result = "Major Mismatch in Rows"
  }
  
  return(result)
  
}



# Compare Data for Explosives Manufacturing -------------------------------

# Load table created in dashboard
dash_expl <- read_csv("qc/from_dashboard/DMR_2024_Explosives-Manufacturing_Dashboard.csv")

# Load table created in DMR Cleaning for OWA project, which has been QCd
owa_expl <- read_excel("qc/from_owa_proj/DMR_2024_ND-as-half_Explosives_Manufacturing_2026.09.16.xlsx")


compare_datasets(dashboard_df = dash_expl, 
                 owa_df = owa_expl)


# Compare Aluminum Data -------------------------------

# Load table created in dashboard
dash_alum <- read_csv("qc/from_dashboard/DMR_2024_Aluminum-Data_Dashboard.csv")

# Load table created in DMR Cleaning for OWA project, which has been QCd
owa_alum <- read_excel("qc/from_owa_proj/DMR_2024_ND-as-half_Aluminum_2026.09.16.xlsx",
                       guess_max = 100000)

compare_datasets(dashboard_df = dash_alum, 
                 owa_df = owa_alum)
