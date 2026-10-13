

server <- function(input, output, session) {
  
  # Initialize waiter function
  w = Waiter$new(html = query_loading_screen)
  
  # Pull demo data if the user has toggled the 
  demo_mode_dat <- eventReactive(input$run_btn, {
    
    # Use demo data if user toggled the Demo/Debug Mode option in the UI
    req(input$demo_mode)

    df_loc <- input$select_demo_dataset

    df <- read_csv(df_loc)%>%
      mutate(psc_code = as.character(psc_code)) # Modify datatype to match
    
    return(df)

    }
    )

  # Build query based on user inputs -------------------------------------------
  
  user_query <- reactive({

    message("--- Building SQL Query Object ---")
    
    # Check database connection. Return error if 
    if (exists("con") == FALSE || is.null(con)) {
      message("ERROR: Unable to connect to database.")
      return(NULL)
    }
    
    ## Get Selected Year -------------------------------------------------
    selected_year <<- input$mp_year
    
    ## Get Non-Detect Handling Method ------------------------------------------
    selected_nd_method <- input$nd_handling
    
    ## Lazy shortcut for now that can be improved by refining database
    selected_tab <- case_when(selected_year == "2024" & selected_nd_method == "Half" ~ 'DMR_2024_NDasHalf',
                              selected_year == "2024" & selected_nd_method == "Zero" ~ 'DMR_2024_NDasZero')
    
    ## Initialize Query --------------------------------------------------------
    # Connect to table in database with dplyr and define starting point for query
    
    query <- tbl(con,
                 selected_tab)%>%
      filter(facility_type_indicator != "POTW")
    
    
    # Permit Type -------------------------------------------------------------

    if(input$permit_type_select == "NPD"){
      query <- query%>%
        filter(permit_type == "NPD")
    } else if(input$permit_type_select == "GPC"){
      query <- query%>%
        filter(permit_type == "NPD")
    } else{
      query <- query
    }
    
      
    
    
    ## PSC Codes -------------------------------------------------------------------
    
    if (!is.null(input$psc_select) && length(input$psc_select) > 0) {
      # Get selected PSC Codes
      selected_psc <<- input$psc_select%>%
        str_extract("^\\d\\d\\d")
      
      # Update selection box in the "Visualizations" tab to only show PSCs selected by user
      updateSelectInput(session, "select_plot_pscs", choices = input$psc_select) 
      
    } else {
      selected_psc <<- NULL
    }
    
    ## State -------------------------------------------------------------------
    if (!is.null(input$state_select) && length(input$state_select) > 0) {
      
      # Get selected states in vector format
      selected_states <<- input$state_select
      
      # Append state filter to query from above
      # Uses the unquote injection (!!) for security against malicious or accidental SQL injection 
      query <- query %>% filter(state %in% !!selected_states)
    }
    
    ## EPA Region --------------------------------------------------------------
    if (is.null(input$epa_region_select) == FALSE && length(input$epa_region_select) > 0) {
      
      # Get EPA Region; remove the preceding "Region " text 
      epa_regions <<- str_remove(input$epa_region_select, "Region ")#%>%#as.numeric()
      
      query <- query %>% filter(epa_region %in% !!epa_regions)
    }

    ## Pollutant -------------------------------------------------------------------
    if (!is.null(input$pollutant_select) && length(input$pollutant_select) > 0) {
      
      # Get selected states in vector format
      selected_pollutants <<- input$pollutant_select
      
      # Update selection box in the "Visualizations" tab to only show pollutants selected by user
      updateSelectInput(session, "select_boxplot_polls", choices = input$pollutant_select) 
      
      
      # Append state filter to query from above
      # Uses the unquote injection (!!) for security against malicious or accidental SQL injection 
      query <- query %>% filter(pollutant_description %in% !!selected_pollutants)
    }
    
    # Return Query with Additions from User Selections ------------------------
    
    return(query)
    })
  
  
  # Generate and Preview SQL query --------------------------------------------

  output$query_text <- renderText({
    # Require that user_query() has been executed before updating
    req(user_query())
    
    # Translate from dbplyr to SQL and print to console and dashboard
    sql_string <- user_query() %>% sql_render()
    message("Generated SQL Script:\n", sql_string, "\n")
    
    return(as.character(sql_string)) # Not sure that this is needed
  })
  
  # Query Database and Get Resulting Subset of DMR Data -----------------------

  final_data <- eventReactive(input$run_btn, {
    
    req(user_query())
    
    # If in demo/debug mode, use pre-generated data to speed up query process.
    # Otherwise, query the actual database. 
    if (input$demo_mode == TRUE){
      message("--- Pulling Demo Data ---")
      message(input$demo_mode)
      message(length(input$select_demo_dataset))
      
      df <- demo_mode_dat()
      
      message("--- Demo Data Compiled ---")
      
      
    } else{
      w$show() # Show waiter loading circle
      
      message("--- Initializing Database Call ---")
      # Require that user_query() has been executed before running
      
      
      message("Sending query to Database...")
      xwalk_psc <- dbReadTable(con, "xwalk_psc_pollcodes")%>%
        reframe(.by = c(npdes_permit_number, 
                        pollutant_code,
                        psc_code))%>%
        tibble()%>%
        mutate(pollutant_code = as.numeric(pollutant_code))
      
      ## Query database  ---------------------------------------------------------
      df <- user_query()%>%
        collect()
      
      # Append PSC Codes --------------------------------------------------------
      message("Appending PSC Codes")
      df<-df%>%
        left_join(xwalk_psc, 
                  by = join_by(npdes_permit_number,
                               pollutant_code),
                  relationship = "many-to-many")
      
      # Filter for PSC Codes Defined by User ------------------------------------
  
      # PSC Codes aren't included in the database table, so we can't filter them
      # out with the SQL query. So instead we join and filter out here. 
      if (is.null(selected_psc) == FALSE && length(selected_psc) > 0) {
        df <- df%>%
          filter(psc_code %in% selected_psc)
      } else{
      }
      
      message("Query complete! Total rows retrieved: ", nrow(df))
      message("\nData snapshot columns: ", paste(colnames(df), collapse = ", "))
      
      # Process the Resulting DMR Data for Summarization ------------------------
      # Handles removal of POTWs and processes non-detects
      df <- fn_process_dmr_dat(df)
      
      # Hide the loading circle
      w$hide() 
      
    }
    
    # Return final processed data ---------------------------------------------
    return(df)
    
  })
  
  # Summarize Data ----------------------------------------------------------
  summarized_data_psc <- eventReactive(input$run_btn, {

    message("\nSummarizing Data by PSC Code")
    
    summary_dat_psc <- fn_get_summary_stats(dmr_dat_mod = final_data())
    
    message("\nSummarization complete!")

    return(summary_dat_psc)
    
  })
  
  summarized_data_sic <- eventReactive(input$run_btn, {
    
    message("\nSummarizing Data by SIC Code")
    
    summary_dat_sic <- fn_get_sic_summary_stats(dmr_dat_mod = final_data())
  
    message("\nSummarization complete!")
    
    return(summary_dat_sic)
    
  })
  
  
  

# Outputs -----------------------------------------------------------------

## Data Tables -------------------------------------------------------------
  
  # Complete Data Table
  output$results_table <- DT::renderDataTable({
    req(final_data())
    DT::datatable(
      final_data(),
      options = list(
      scrollX = TRUE,     # Allow horizontal scrolling
      pageLength = 10,    # Limits vertical rendering to 10 rows at a time
      deferRender = TRUE  # Boosts performance by only loading visible rows
      ),
      rownames = FALSE      # Remove unnecessary row names
    )
  }
  )
  
  # Summary Data Table - by PSC Code
  output$summary_table_psc <- DT::renderDataTable({
    req(summarized_data_psc())
    
    DT::datatable(
      summarized_data_psc(),
      options = list(
        scrollX = TRUE,     # Allows horizontal scrolling
        pageLength = 10,    # Limits vertical rendering to 10 rows at a time
        deferRender = TRUE  # Boosts performance by only loading visible rows
      ),
      rownames = FALSE      # Remove unnecessary row names
    )
  }
  )
  
  
  # Summary Data Table - by SIC Code
  output$summary_table_sic <- DT::renderDataTable({
    req(summarized_data_sic())
    
    DT::datatable(
      summarized_data_sic(),
      options = list(
        scrollX = TRUE,     # Allows horizontal scrolling
        pageLength = 10,    # Limits vertical rendering to 10 rows at a time
        deferRender = TRUE  # Boosts performance by only loading visible rows
      ),
      rownames = FALSE      # Remove unnecessary row names
    )
  }
  )
  
  
## Download Full DMR Data (after query) -----------------------------------------------
  
  # Download CSV
  output$download_full_data_csv <- downloadHandler(
    filename = fn_full_dat_filename(file_extension = ".csv"),
    
    content = function(file) {
      write.csv(final_data(), file, row.names = FALSE)
    }
  )

## Download Summary Data ---------------------------------------------------

  # Download as CSV
  output$download_summary_data_csv <- downloadHandler(
    filename = fn_summ_dat_filename(file_extension = ".csv"),
    
    content = function(file) {
      write.csv(summarized_data_psc(), file, row.names = FALSE)
    }
  )  
  
## Download as XLSX ----------------------------------------------------------
  
  output$download_summary_data_xlsx <- downloadHandler(
    filename = fn_summ_dat_filename(file_extension = ".xlsx"),
    
    content = function(file) {
      
      # Define custom style
      
      # Header style: Bold text, white font, ERG-green background, centered text
      header_style <- createStyle(
        fontName = "Calibri",
        fontSize = 11,
        fontColour = "#FFFFFF",
        halign = "center",
        valign = "center",
        fgFill = "#339846" # ERG Green
      )
    
      # Create a workbook object based on the export template file
      # Eventually I'd like to include the user's query info here, but that will
      # require additional LOE
      wb <-loadWorkbook("inputs/export_template.xlsx")
      
      # Add a worksheet
      addWorksheet(wb, sheetName = "DMR Summary")

      # Write data
      writeData(wb, "DMR Summary", summarized_data_psc(), headerStyle = header_style, keepNA = TRUE, na.string = "NA")
      
      # Automatically set column widths
      setColWidths(wb, 'DMR Summary', cols = 1:length(summarized_data_psc()), widths = "auto")
      
      # Save workbook to temporary location ("file") defined in function
      saveWorkbook(wb, 
                   file, 
                   overwrite = TRUE)
    }
  )  
  
  
  # Visualizations ----------------------------------------------------------
  
  ## Top 10 pollutants in the selected PSC Code(s) ------------------------------
  
  # Calculate number of rows/facets dynamically
  plot_height <- reactive({
    req(summarized_data_psc())
    
    n_pscs <- length(unique(summarized_data_psc()$`PSC Description`))
    
    if(n_pscs == 1){
      height <- n_pscs * 2 * 250 
    } else{
      height <- n_pscs * 250 
    }
    
    cat(file=stderr(), "Count of PSCs Selected", n_pscs, "\n")
    
    return(height) # 250 pixels per row as a baseline
  })
  
  
  output$summary_bar <- renderPlot({
    req(summarized_data_psc())

    selected_psc_desc <<- str_remove_all(input$select_plot_pscs, "\\d\\d\\d\\s\\-\\s")
    
    message(selected_psc_desc)
    
    
    if(length(selected_psc_desc) == 0){
      summarized_data_psc()%>%
        summarize_top_polls()
      
    } else{
      summarized_data_psc()%>%
        filter(`PSC Description` %in% selected_psc_desc)%>%
        summarize_top_polls()
      
    }
    },
    width = "auto",
    height = plot_height,
    res = 100)
  
  output$pollutant_boxplot <- renderPlot({
    
  })
  
  
  
# 
# ## Text Outputs ------------------------------------------------------------
#   
#   
#   # PSC Code(s)
#   output$selected_psc_list <- if(!is.null(selected_psc) && length(selected_psc) > 0){
#     renderText({selected_psc})
#   } else {
#     renderText("All")
#   }
#   
#   
#   # States
#   output$selected_state_list <- if(!is.null(selected_states) && length(selected_states) > 0){
#     renderText({selected_states})
#     } else {
#       renderText("All")
#     }
#   
#   
  

# Navigation --------------------------------------------------------------


## Send User to "Data Table" pane once they click "Query Database" button --------------

  observeEvent(input$run_btn, {
    updateNavbarPage(session = getDefaultReactiveDomain(),
                     inputId = "dmr_tool",
                     selected = "panel2")
    })
  
## Send User to "Data Table" pane once they click "Summarize Data" button --------------
  
  observeEvent(input$summary_btn, {
    updateNavbarPage(session = getDefaultReactiveDomain(),
                     inputId = "dmr_tool",
                     selected = "panel3")
  })
  
  
}


