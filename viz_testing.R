library(readxl)
library(forcats)
library(tidytext)
library(scales)
library(tidyverse)

dat_multi <- read_csv("inputs/demo_dat/DMR_2024_Four_PSC_Codes_ND_as_Zero_Summarized.csv")


get_n_pscs <- function(summarized_dat){
  
  n_pscs <- length(unique(summarized_dat$`PSC Description`))
  return(n_pscs)
  
}

get_n_pscs(dat_multi)


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
               scales = "free",
               ncol = 2)+
    geom_bar(stat = "identity",
             fill = "steelblue")+
    scale_x_discrete(labels = ~ label_wrap(30)(str_remove(.x, "___.+"))) +
    scale_y_continuous(labels = label_comma())+
    theme(axis.text.x = element_text(angle = 35, hjust = 1))+
    labs(
      x = "Pollutant Descriptiom",
      y = "",
      title = "Mean Concentration (mg/L) of the Top 10 Pollutants per Point Source Category")
}

summarize_top_polls(dat_multi)
