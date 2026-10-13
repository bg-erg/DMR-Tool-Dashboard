
ui <- page_navbar(
  
  # title = tagList(
  #   brand_use_logo(brand_data, name = "small"),
  #   "DMR Annual Review Tool"),
  
  title = "ERG - DMR Annual Review Tool",
  
  id = "dmr_tool",
  
  theme = bs_theme(preset = "united",
                   navbar_bg = "#339846"),

  
  # Page 1: Input Criteria -----------------------------------------------------
  nav_panel(
    useWaiter(),
    
    title = "Criteria",
    value = "panel1",
    icon = icon("sliders"),
    
    ## Page ----------
    fluidPage( 
      # Show spinner
      layout_columns(
    ## Card 1: Search Criteria ---------------------------------------------
        card(
          card_header("Filter Parameters"),
          
          # Demo/Debug Mode ---------------------------------------------------------

          # Toggle demo/debug mode
          input_switch(
            id = "demo_mode", 
            label = "Demo/Debug Mode"),
          
          # Conditional panel that gives additional options
          conditionalPanel(
            condition = "input.demo_mode == true",
            selectInput(
              inputId = "select_demo_dataset",
              label = "Select Demo Dataset to Use",
              choices = list("2024 Explosives Industry Data - ND as Zero" = "inputs/demo_dat/DMR_2024_Full_Explosives_ND_as_Zero.csv",
                             "2024 Aluminum Data - ND as Zero" = "inputs/demo_dat/DMR_2024_Full_Aluminum_ND_as_Zero.csv",
                             "2024 Explosives Industry Data - ND as Half MDL" = "inputs/demo_dat/DMR_2024_Full_Explosives_ND_as_Half.csv",
                             "2024 Aluminum Data - ND as Half MDL" = "inputs/demo_dat/DMR_2024_Full_Aluminum_ND_as_Half.csv",
                             "2024 Data for PSC Codes 434, 435, 419, and 420 - ND as Zero" = "inputs/demo_dat/DMR_2024_Four_PSC_Codes_ND_as_Zero.csv"
                             
                             ), 
              multiple = FALSE,
              selectize = TRUE,
              width = "70%")#,
            
            # actionButton(
            #   inputId = "confirm_demo_dat",
            #   label = "Get Demo Data",
            #   class = "btn-primary", # Make full width
            #   icon = icon("database"))
            ),
          
          

          ### Select How to Handle NDs ------------------------------------------------
          
          
          radioButtons( 
            inputId = "nd_handling", 
            label = "Set Non-Detects Equal To", 
            choices = list("\u00BD Detection Limit" = "Half",
                           "Zero" = "Zero"),
            inline = TRUE
            ), 

          ### Select Year -------------------------------------------------------
          selectInput(inputId = "mp_year",
                      label = "Select Monitoring Year", 
                      selected = "2024",
                      width = "200px",
                      choices = list("2024")),
          
          ### Select Permit Types -----------------------------------------------------
          selectInput(inputId = "permit_type_select", 
                      label = "Select Permit Type",
                      selected = "NPD",
                      width = "300px",
                      choices = list("NPD - NPDES Individual Permit" = "NPD",
                                     "GPC - General Permit Covered Facility" = "GPC",
                                     "All Permit Types" = "All")),
          
          hr(),
          
          ### Select PSC Code(s) ---------------------------------
          selectizeInput(
            inputId = "psc_select",
            label = "Select PSC Code(s)",
            choices = user_choices$psc_choices,
            multiple = TRUE,
            options = list(placeholder = 'All Point Source Categories')
            ),
          
          ### Select State(s) ---------------------------------
          selectizeInput(
            inputId = "state_select",
            label = "Select State(s):",
            choices = user_choices$state_choices,
            multiple = TRUE,
            options = list(placeholder = 'All States')
          ),
          
          ### Select Pollutant(s) -----------------------------
          selectizeInput(
            inputId = "pollutant_select",
            label = "Select Pollutant Name(s):",
            choices = user_choices$pollutant_choices,
            multiple = TRUE,
            options = list(placeholder = 'All Pollutants')
          ),
          
          #### Input NAICS Code(s) -------------------
          # textInput(
          #   inputId = "naics_input",
          #   label = "Enter NAICS Code(s) (separated by commas):",
          #   placeholder = "e.g., 325110, 325120"
          # ),
          
         #### Input SIC Code(s) -------------------
         # selectizeInput(
         #      inputId = "sic_input",
         #      label = "Select SIC Code(s)",
         #      choices = user_choices$sic_choices,
         #      multiple = TRUE,
         #      #server = TRUE,
         #      options = list(placeholder = 'Begin typing to search for SIC Codes')
         #    ),
          
         hr(), # Horizontal Line
         
         ### Finalize Query and Run---------------------------------------------

         
         #p("Query Preview"),
         #verbatimTextOutput("query_text"),
         
         actionButton(
           inputId = "run_btn",
           label = "Get Data",
           class = "btn-primary btn-lg", # Make full width
           icon = icon("database")
           )
         )
        )
      )
    ),
         
  # Page 2: Data Table -----------------------------------------------------
  nav_panel(
    useWaiter(),
    
    title = "Data Table",
    value = "panel2",
    icon = icon("table"),
    fluidPage( 
      h2("Data Table"),
      
      hr(), # Horizontal Line
      
      actionButton(
        inputId = "summary_btn",
        label = "Summarize Data",
        class = "btn-primary", # Make full width
        icon = icon("list")),
      
      downloadButton(
        outputId = "download_full_data_csv",
        label = "Download as CSV File",
        class = "btn-info",
        icon = shiny::icon("file-csv")),
      
      p("Note: It may take several seconds for the download to begin for large datasets."),
      
      hr(), # Horizontal Line
      
      
      DT::dataTableOutput("results_table"),
      
      DT::dataTableOutput("demo_table")
      
      )
    ),
  
  # Page 3: Summary Table -----------------------------------------------------
  nav_panel(
    useWaiter(),
    
    title = "Summary Table",
    value = "panel3",
    icon = icon("table"),
    
    fluidPage(
      h2("Summmary Data Table"),
     # h4("Selected Criteria"),
      # # PSC Code(s)
      # p("Selected PSC Code(s):"),
      # textOutput("selected_psc_list", inline = TRUE), 
      # 
      # # States
      # p("\nSelected State(s):"),
      # textOutput("selected_state_list", inline = TRUE), 
      
      # Download Buttons
      downloadButton(outputId = "download_summary_data_xlsx", 
                     label = "Download as Excel File",
                     class = "btn-info"),
     
      downloadButton(outputId = "download_summary_data_csv", 
                     label = "Download as CSV File",
                     class = "btn-info",
                     icon = shiny::icon("file-csv")),
      hr(),
     
     radioButtons( 
       inputId = "summary_mode", 
       label = "Summarize By:", 
       choices = list("PSC Code" = "psc_summary",
                      "SIC Code" = "sic_summary"),
       selected = "psc_summary",
       inline = TRUE
     ), 
     
     conditionalPanel(
       condition = "input.summary_mode == 'psc_summary'",
       DT::dataTableOutput("summary_table_psc")
     ),

     conditionalPanel(
       condition = "input.summary_mode == 'sic_summary'",
       DT::dataTableOutput("summary_table_sic")
       
     )
     
     
    )
    
    ),
  
  # Visualizations ----------------------------------------------------------
  nav_panel(
    title = "Visualizations",
    value = "panel4",
    icon = icon("chart-column"),
    
    fluidPage(
      
      # Select Plot to Display
      selectizeInput(
        inputId = "plot_type",
        label = "Select Visualization to Display",
        choices = c("Top 10 Pollutants per PSC Code" = "top_pscs",
                    "Distribution of Pollutant Concentrations Across PSC Codes" = "poll_dist_boxplot"),
        multiple = FALSE),
      
      # Select PSC Codes to Display
      conditionalPanel(
        condition = "input.plot_type == 'top_pscs' || input.plot_type == 'poll_dist_boxplot'",
        selectizeInput(
          inputId = "select_plot_pscs",
          label = "Select PSC Codes to Display",
          choices = "",
          multiple = TRUE)),
      
      # Select Pollutants to Display
      conditionalPanel(
        condition = "input.plot_type == 'poll_dist_boxplot'",
        selectizeInput(
          inputId = "select_boxplot_polls",
          label = "Select Pollutant(s) to Display",
          choices = "",
          multiple = TRUE)),
      
      # Create Visualizations
      actionButton(
        inputId = "viz_btn",
        label = "Create Visualization(s)",
        class = "btn-primary", # Make full width
        icon = icon("chart-column")),
      
      
      hr(),
      
      # Bar Plot of Summary Data
      conditionalPanel(
        condition = "input.plot_type == 'top_pscs'",
        plotOutput("summary_bar")),
      
      # Boxplot of Pollutants
      conditionalPanel(
        condition = "input.plot_type == 'poll_dist_boxplot'",
        plotOutput("pollutant_boxplot"))
      
      
      )
    )
  )




