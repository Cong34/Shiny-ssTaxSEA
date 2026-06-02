#
# TBA
#

library(shiny)
library(tidyverse)
library(ggrepel)
library(bslib)
library(bsicons)
library(openxlsx2)
library(DT)
library(TaxSEA)
library(ComplexHeatmap)
library(circlize)


# Helper to display P values and FDR values in scientific notation, without changing the dataframe values to text formatted in scientific notation.
# At the time of writing, there is an open issue for this functionality in DT: https://github.com/rstudio/DT/issues/938
jsExponential <- c(
  "function(row, data, displayNum, index){",
  "  for (var i = 3; i <= 4; i++) {",
  "    var x = data[i];",
  "    if (!isNaN(parseFloat(x))) {",
  "      $('td:eq(' + i + ')', row).html(parseFloat(x).toExponential(2));",
  "   }",
  "  }",
  "}"
)

# Helper to detect header rows in supplied (or example) data
has_header <- function(file_path) {
  if (tools::file_ext(file_path) == "csv") {
    first_row <- read.csv(file_path, header = FALSE, nrows = 1, stringsAsFactors = FALSE)
  } else if (tools::file_ext(file_path) %in% c("xlsx", "xlsm")) {
    first_row <- read_xlsx(file_path, col_names = FALSE, rows = 1)
  }
  
  # Check if all columns in the first row are numeric, except for the first column
  is_numeric <- sapply(first_row[-1], is.numeric)
  
  if(all(is_numeric)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

valid_input_format <- function(suppliedData) {
  if (ncol(suppliedData) != 4) {
    showModal(modalDialog(
      title = "Input Error",
      "Incorrect number of input columns. Expecting exactly 4; Taxa, log 2-fold changes, P value, and Padj or FDR."
    ))
    return(FALSE)
  } else if (!is.character(suppliedData[[1]])) {
    showModal(modalDialog(
      title = "Input Error",
      "First column (Taxa) must be text"
    ))
    return(FALSE)
  } else if (!is.numeric(suppliedData[[2]])) {
    showModal(modalDialog(
      title = "Input Error",
      "Second column (log 2-fold change) must be numeric"
    ))
    return(FALSE)
  } else if (!is.numeric(suppliedData[[3]]) || min(suppliedData[[3]]) < 0 || max(suppliedData[[3]]) > 1) {
    showModal(modalDialog(
      title = "Input Error",
      "Third column (P value) must be a numeric value between 0 and 1"
    ))
    return(FALSE)
  } else if (!is.numeric(suppliedData[[4]]) || min(suppliedData[[4]]) < 0 || max(suppliedData[[4]]) > 1) {
    showModal(modalDialog(
      title = "Input Error",
      "Fourth column (Padj / FDR) must be a numeric value between 0 and 1."
    ))
    return(FALSE)
  } else {
    return(TRUE)
  }
}

ui <- page_sidebar(
  title = "Shiny TaxSEA",
  sidebar = sidebar(
    width = "310px",
    fileInput(
      "file",
      label = tooltip(
        trigger = list(
          "Differential abundance data for analysis",
          bs_icon("question-circle")
        ), "Browse for a Microsoft Excel️ (R) or CSV file with columns for: Taxa, log 2-fold change, P value, and Padj / FDR"
      ) 
    ),
    fileInput(
      "count_file",
      label = tooltip(
        trigger = list(
          "Count table for ssTaxSEA",
          bs_icon("question-circle")
        ),
        "Browse for a CSV or Excel file with Taxa as rows and patients as columns"
      )
    ),

    selectInput(
      "database_selection",
      "Select Database to display",
      choices = list("Metabolites" = "Metabolite_producers", "Health Associations" = "Health_associations", "BacDive bacterial physiology" = "BacDive_bacterial_physiology")
    ),
  
    actionButton(
      "loadSampleData",
      icon = icon("bolt"),
      "Analyse sample data",
    ),
    downloadButton(
      "downloadSampleData",
      icon = icon("cloud-download"),
      "Download sample data"
    ),
    downloadButton(
      "downloadSampleCountData",
      icon = icon("cloud-download"),
      "Download sample count data"
    ),
    div(
      class = "text-center",
      tags$a(
        href = "https://github.com/timrankin/Shiny-TaxSEA/issues",
        target = "_blank",
        class = "d-flex align-items-center justify-content-center text-decoration-none",
        span("Report a bug", class = "me-2"),
        bs_icon("bug-fill")
      )
    )
  ),
  
  navset_tab(
    
    # ── Tab 1: existing TaxSEA 1 UI (unchanged) ──────────────────
    nav_panel(
      "TaxSEA",
      layout_columns(
        card(
          card_header(
            class = "d-flex align-items-center",
            "Bar Plot",
            tags$span(style = "margin-left: 5px"),
            tooltip(bs_icon("info-circle"), "..."),
            uiOutput("downloadBarPlotUi")
          ),
          plotOutput("barPlot")
        ),
        card(
          card_header(
            class = "d-flex align-items-center",
            "Volcano Plot",
            tags$span(style = "margin-left: 5px"),
            tooltip(bs_icon("info-circle"), "..."),
            uiOutput("downloadVolcanoPlotUi")
          ),
          plotOutput("volcanoPlot")
        )
      ),
      card(
        card_header(
          class = "d-flex align-items-center",
          "TaxSEA Results",
          uiOutput("downloadDataTableUi")
        ),
        DTOutput("table")
      )
    ),
    
    # ── Tab 2: new TaxSEA 2 UI ────────────────────────────────────
    nav_panel(
      "ssTaxSEA",
      layout_columns(
        card(
          card_header(
            class = "d-flex align-items-center",
            "Heatmap",
            tags$span(style = "margin-left: 5px"),
            div(
              class = "d-flex align-items-center gap-2 ms-auto",
              tags$input(
                type = "checkbox",
                id   = "show_col_names",
              ),
              tags$label(
                `for` = "show_col_names",
                "Show sample IDs"
              )
            ),
            div(
              class = "d-flex align-items-center gap-2 ms-3",
              "Taxon sets",
              numericInput("taxon_sets_amount",
                           label = NULL, value = 8, min = 1, max = 20, step = 1,
                           width = "60px"),
            ),
            uiOutput("downloadHeatmapUi")
          ),
          plotOutput("heatmap")
        ),
      ),
      card(
        card_header(
          class = "d-flex align-items-center",
          "ssTaxSEA Results",
          uiOutput("downloadDataTable2Ui")
        ),
        DTOutput("table2")
      )
    )
  )
)

server <- function(input, output, session) {
  # Notifications variable, used if > 8 rows are selected in table
  notificationIds <- NULL
  
  # Reactive value to store data from either user supplied file or example data
  data <- reactiveVal(NULL)
  countData <- reactiveVal(NULL) # Add count table for ssTaxSEA
  
  barPlotReady <- reactiveVal(FALSE)
  volcanoPlotReady <- reactiveVal(FALSE)
  dataTableReady <- reactiveVal(FALSE)
  dataTable2Ready <- reactiveVal(FALSE)
  table2_render_id <- reactiveVal(0)
  heatmapReady <- reactiveVal(FALSE)
  taxon_sets_amount <- reactiveVal(6)
  
  observeEvent(input$database_selection, {
    table2_render_id(table2_render_id() + 1)
  })
  
  # TODO: implementing a check to display a warning if there are no taxon sets w/FDR <0.02. 
  #       This probably belongs better in the TaxSEA function, otherwise DT
  
  # Read uploaded file
  observeEvent(input$file, {
    req(input$file)
    
    if (tools::file_ext(input$file$datapath) == "csv") {
      suppliedData <- read.csv(input$file$datapath, header = has_header(input$file$datapath))
      
    } else if (tools::file_ext(input$file$datapath) %in% c("xlsx", "xlsm")) {
      suppliedData <- read_xlsx(input$file$datapath, col_names = has_header(input$file$datapath))
    }
    
    if(valid_input_format(suppliedData)) {
      return(data(suppliedData))
    } else {
      return()
    }
  })
  # Read countfile: 
  observeEvent(input$count_file, {
    req(input$count_file)
    
    if (tools::file_ext(input$count_file$datapath) == "csv") {
      suppliedCount <- read.csv(input$count_file$datapath, header = TRUE, row.names = 1)
    } else if (tools::file_ext(input$count_file$datapath) %in% c("xlsx", "xlsm")) {
      suppliedCount <- read_xlsx(input$count_file$datapath, col_names = TRUE, header = TRUE, row.names = 1)
    }
    countData(suppliedCount)
  })
  # Handle example data button click & load example data
  observeEvent(input$loadSampleData, {
    exampleData  <- read.csv("test_input.csv", 
                             header = has_header("test_input.csv"))
    exampleCount <- read.csv("Shiny-ssTaxSEA_count_table.csv", 
                             header = TRUE, row.names = 1)
    data(exampleData)
    countData(exampleCount)
  })
  # adjusting the taxonsets input: 
  observeEvent(input$increase_taxon, {
    updateNumericInput(session, "taxon_sets_amount", value = input$taxon_sets_amount + 1)
  })
  
  observeEvent(input$decrease_taxon, {
    updateNumericInput(session, "taxon_sets_amount", value = max(1, input$taxon_sets_amount - 1))
  })
  
  ############################################################################### 
  # Run TaxSEA on data
  ############################################################################### 
  taxseaResults <- reactive({
    # Make sure data has been supplied in a valid format
    req(data())

    # Get taxon ranks from user supplied data
    taxonRanks <- setNames(data()[[2]], data()[[1]])
    
    results <- TaxSEA(taxonRanks)
    
    # Drop column 5 from all results.
    results$Metabolite_producers <- results$Metabolite_producers[, -5]
    results$Health_associations <- results$Health_associations[, -5]
    results$BacDive_bacterial_physiology <- results$BacDive_bacterial_physiology[, -5]
    # results$BugSigDB <- results$BugSigDB[, -5]
    
    # Make results presentable
    # TODO: Only apply this function to disease & metabolites - bsdb requires its own to remove hyphens etc.
    results <- lapply(results, function(df) {
      # Remove rownames
      rownames(df) <- NULL
      
      # Change to readable column names
      colnames(df) <- c("Taxon Set", "Median rank of set members", "P Value", "FDR", "Taxon Set Members")
      
      # Make taxon set names presentable
      df$`Taxon Set` <- df$`Taxon Set` %>%
        str_replace("_(.)", ": \\1") %>%
        str_replace("(: )(.)", toupper) %>%
        str_replace_all("_", " ")
      
      # Make taxon set members presentable
      df$`Taxon Set Members` <- df$`Taxon Set Members` %>%
        str_replace_all("_", " ")
      
      return(df)
    })
    return(results)
  })
  
  # 'Debounce' the clicks on table rows, so we don't redraw the plots too frequently
  debounced_selection <- debounce(
    reactive(input$table_rows_selected),
    millis = 500  # 500 ms delay
  )
  
  # Render the bar plot
  barPlot <- reactive({
    # Make sure data has been supplied and in a valid format
    req(taxseaResults())
    
    # Store selected rows, even if this is 0
    selected_rows <- debounced_selection()
    
    if (is.null(selected_rows)) {
      # Plot top 6 results based on -log10 FDR values
      dataForPlot <- taxseaResults()[[input$database_selection]] %>%
        mutate(negativeLog10FDR = -log10(taxseaResults()[[input$database_selection]][[4]])) %>%
        arrange(desc(negativeLog10FDR)) %>%
        slice_head(n = 8)
    } else if (length(selected_rows) <= 8) {
      # Plot the selected rows
      dataForPlot <- taxseaResults()[[input$database_selection]][selected_rows, ] %>%
        mutate(negativeLog10FDR = -log10(taxseaResults()[[input$database_selection]][selected_rows, ][[4]])) %>%
        arrange(desc(negativeLog10FDR))
    } else {
      # Display the first 8 user selected taxon sets, together with a warning
      dataForPlot <- taxseaResults()[[input$database_selection]][selected_rows[1:8], ] %>%
        mutate(negativeLog10FDR = -log10(taxseaResults()[[input$database_selection]][selected_rows[1:8], ][[4]])) %>%
        arrange(desc(negativeLog10FDR))
      
      notificationId <<- showNotification(
        "⚠️ Bar plot limited to a max of 8 Taxon Sets. Your first 8 selections are displayed, deselect some in order to add more.",
        duration = 10,
        closeButton = TRUE,
        type = "warning"
      )
    }
    
    plot <- ggplot(dataForPlot, aes(x = negativeLog10FDR, y = reorder(str_wrap(`Taxon Set`, width = 34), negativeLog10FDR))) +
      geom_col(fill = "#00aedb") +
      labs(
        x = expression(-log[10] ~ FDR),
        y = "Taxon Sets"
      ) +
      geom_vline(xintercept = -log10(0.1), linetype = 5) +
      theme_classic() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
    barPlotReady(TRUE)
    return(plot)
  })
  
  # Output the bar plot
    output$barPlot <- renderPlot({
      barPlot()
    })
  
  # Render the volcano plot
    volcanoPlot <- reactive({
      req(data())
      req(taxseaResults())
      
      dataForPlot <- data()
      
      # Store last selected row, it will be NULL if none are selected
      last_row_selected <- debounced_selection()[length(debounced_selection())]
      
      # If no selection is made, take the taxon set members from the top TaxSEA result. Otherwise, use the last selection.
      if (is.null(last_row_selected)) {
        taxa_of_interest <- unlist(strsplit(taxseaResults()[[input$database_selection]][[5]][1], ", "))
        plot_title <- taxseaResults()[[input$database_selection]][[1]][1]
      } else {
        taxa_of_interest <- unlist(strsplit(taxseaResults()[[input$database_selection]][[5]][last_row_selected], ", "))
        plot_title <- taxseaResults()[[input$database_selection]][[1]][last_row_selected]
      }
      
      # TODO: Support hyphens as well as underscores (input may also be supplied with spaces already)
      taxa_of_interest <- str_replace(taxa_of_interest, " ", "_")
      
      dataForPlot$is_of_interest <- dataForPlot[[1]] %in% taxa_of_interest
      
      label_data <- dataForPlot[dataForPlot$is_of_interest != FALSE &
                                  dataForPlot[[3]] < 0.05, ]
      label_data$Taxa <- gsub("_", " ", label_data$Taxa)
      label_data$Abbreviated_taxa <- sub("^([A-Za-z])[a-z]+\\s", "\\1. ", label_data$Taxa)
      
      plot <- ggplot(
        dataForPlot,
        aes(
          x = dataForPlot[, 2],
          y = -log10(dataForPlot[, 3]),
          color = is_of_interest,
          alpha = is_of_interest
        )
      ) +
        geom_point(aes(size = is_of_interest)) +
        theme_classic() +
        labs(title = plot_title,
             x = "Input Ranks",
             y = expression(-log[10] ~ FDR)) +
        scale_alpha_manual(values = c("FALSE" = 0.3, "TRUE" = 1)) +
        scale_color_manual(values = c("FALSE" = "grey50", "TRUE" = "steelblue")) +
        scale_size_manual(values = c("FALSE" = 2, "TRUE" = 4)) +
        geom_text_repel(
          data = label_data,
          aes(
            x = label_data[, 2],
            y = -log10(label_data[, 3]),
            label = Abbreviated_taxa,
            fontface = "italic"
          ),
          size = 5
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "none"
        ) +
        geom_vline(xintercept = 0, linetype = 5)
      
      volcanoPlotReady(TRUE)
      return(plot)
  })
  
    # Output the volcano plot
    output$volcanoPlot <- renderPlot({
      volcanoPlot()
    })
    
  # Render the data table
  output$table = DT::renderDataTable({
    req(taxseaResults())
    updateActionButton(session, "downloadTable", label = "Download", disabled = FALSE)
    table <- datatable(
      taxseaResults()[[input$database_selection]],
      filter = "top",
      options = list (
        searching = FALSE,
        paging = FALSE,
        columnDefs = list(
          list(
            targets = 2, width = "125px"
          ),
          list(
            targets = c(3, 4), width = "70px"
          )
        ),
        rowCallback = JS(jsExponential),
        selection = 'multiple'
      )
    ) %>%
      formatRound(
        columns = "Median rank of set members",
        digits = 3 # TODO: Consider whether it's best to reference the textual column title or by number
        ) %>% 
      formatStyle(
        columns = 5,
        fontStyle = "italic"
      )
    
    dataTableReady(TRUE)
    return(table)
  })

  ############################################################################### 
  # Run ssTaxSEA on data (require a count table)
  ############################################################################### 
  sstaxseaResults <- reactive({
    req(data())
    req(countData())
    
    # Running TaxSEA first:
    # Get taxon ranks from user supplied data
    taxonRanks <- setNames(data()[[2]], data()[[1]])
    
    # Formatting the count matrix:
    count_df <- countData()
    
    suppressWarnings({
      results <- TaxSEA(taxonRanks)
      
      # This function write the most important taxon sets and output in a name list as custom_db for ssTaxSEA
      write_taxonset <- function(df) {
        Taxa_list <- setNames(lapply(df$TaxonSet, function(taxa_string) {
          strsplit(taxa_string, ", ")[[1]]
        }),
        df$taxonSetName
        )
        # Clean up taxon set names to match TaxSEA style
        names(Taxa_list) <- names(Taxa_list) %>%
          str_replace("_(.)", ": \\1") %>%
          str_replace("(: )(.)", toupper) %>%
          str_replace_all("_", " ")
        return(Taxa_list) #The output is a namelist of important taxonsets and all of the bacteria taxa
      }
      
      metabolites_db  <- write_taxonset(results$Metabolite_producers)
      disease_db      <- write_taxonset(results$Health_associations)
      bacdive_db      <- write_taxonset(results$BacDive_bacterial_physiology)
      # Bugsig is not included. 
      
      ssTaxSEA_metabolites <- ssTaxSEA(counts = count_df, custom_db = metabolites_db)
      ssTaxSEA_disease     <- ssTaxSEA(counts = count_df, custom_db = disease_db)
      ssTaxSEA_bacdive     <- ssTaxSEA(counts = count_df, custom_db = bacdive_db)

      # ssTaxSEA_metabolites_cuttoff <- Significant_TaxonSets_cutoff(results$Metabolite_producers, ssTaxSEA_metabolites, input$taxon_sets_amount)
      # ssTaxSEA_disease_cuttoff     <- Significant_TaxonSets_cutoff(results$Health_associations, ssTaxSEA_disease, input$taxon_sets_amount)
      # ssTaxSEA_bacdive_cuttoff     <- Significant_TaxonSets_cutoff(results$BacDive_bacterial_physiology, ssTaxSEA_bacdive, input$taxon_sets_amount)
      # 
      Results <- list(ssTaxSEA_metabolites$scores, ssTaxSEA_disease$scores, ssTaxSEA_bacdive$scores)
      names(Results) <- c("Metabolite_producers", "Health_associations", "BacDive_bacterial_physiology")
      return(Results)
      })
  })

  # Render the data table
  output$table2 = DT::renderDataTable({
    req(sstaxseaResults())
    req(taxseaResults())
    
    Significant_TaxonSets_cutoff <- function(TaxSEA_res, ssTaxSEA_res, x) {
      df_high <- TaxSEA_res[order(TaxSEA_res$`P Value`, decreasing = FALSE),]
      df_cutoff <- head(df_high, x)
      list_cutoff <- df_cutoff$`Taxon Set`

      ssTaxSEA_res <- ssTaxSEA_res[,colnames(ssTaxSEA_res) %in% list_cutoff]
      return(ssTaxSEA_res)
    }
    
    # Use the same database_selection input as TaxSEA tab
    ssTaxSEA_score <- sstaxseaResults()[[input$database_selection]]
    TaxSEA_res     <- taxseaResults()[[input$database_selection]]
    display_res <- Significant_TaxonSets_cutoff(TaxSEA_res, ssTaxSEA_score, input$taxon_sets_amount)
    

    display_df <- as.data.frame(t(display_res))
    
    
    col_indices <- seq_len(ncol(display_df)) + 1
    
    table2 <- datatable(
      display_df,
      filter   = "top",
      rownames = TRUE,
      caption  = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold;",
        "ssTaxSEA enrichment scores — rows: taxon sets, columns: Samples/Patients"
      ),
      options = list(
        searching  = TRUE,
        paging     = TRUE,
        pageLength = 15,
        scrollX    = TRUE,
        columnDefs = list(list(targets = 0, width = "220px"))
      )
    ) %>%
      formatRound(columns = colnames(display_df),
                  digits  = 5)
  
    dataTable2Ready(TRUE)
    return(table2)
  })
  
  heatmapPlot <- reactive({
    req(sstaxseaResults())
    req(taxseaResults())
    
    Significant_TaxonSets_cutoff <- function(TaxSEA_res, ssTaxSEA_res, x) {
      df_high <- TaxSEA_res[order(TaxSEA_res$`P Value`, decreasing = FALSE),]
      df_cutoff <- head(df_high, x)
      list_cutoff <- df_cutoff$`Taxon Set`

      ssTaxSEA_res <- ssTaxSEA_res[,colnames(ssTaxSEA_res) %in% list_cutoff]
      return(ssTaxSEA_res)
    }
    
    ssTaxSEA_score <- sstaxseaResults()[[input$database_selection]]
    TaxSEA_res     <- taxseaResults()[[input$database_selection]]
    
    display_res <- Significant_TaxonSets_cutoff(TaxSEA_res, ssTaxSEA_score, input$taxon_sets_amount)
    scores <- t(display_res)
    
    rownames(scores) <- rownames(scores) %>%
      str_replace("_(.)", ": \\1") %>%
      str_replace("(: )(.)", toupper) %>%
      str_replace_all("_", " ")
    
    col_fun = colorRamp2(
      c(min(scores), 0, max(scores)),
      c("deepskyblue3", "white", "firebrick4")
    )
    
    ht <- Heatmap(scores,
      name = "Enrichment\nScore",
      col = col_fun,
      
      # Row display
      row_names_side = "left",
      row_names_gp = gpar(fontsize = 15),
      row_names_max_width = unit(10, "cm"),

      
      # Column display:
      show_column_names = input$show_col_names,
      column_title      = "Samples",
      column_title_side = "bottom",
      show_column_dend  = TRUE,
      column_names_rot  = 45,
      column_names_gp = gpar(fontsize = 10),
      
      # Clustering: 
      row_dend_side = "right",
      cluster_columns = TRUE,
      clustering_distance_columns = "euclidean",
      clustering_method_columns = "ward.D2",
      
      # Legend
      heatmap_legend_param = list(
        title = "Enrichment Score",
        direction = "vertical",
        at = c(floor(min(scores)), 0, ceiling(max(scores)))
      ))
    return(ht)
  })
  ##############################################################################
  # Adding download function to the UI: 
  ##############################################################################
  # Handle sample data download and count table for ssTaxSEA
  output$downloadSampleData <- downloadHandler(
    filename = function() {"Shiny-TaxSEA_sample_data.csv"},
    content = function(file) {
      file.copy("Shiny-TaxSEA_sample_data.csv", file)
    }
  )
  output$downloadSampleCountData <- downloadHandler(
    filename = function() { "Shiny-ssTaxSEA_count_table.csv" },
    content = function(file) {
      file.copy("Shiny-ssTaxSEA_count_table.csv", file)
    }
  )
  # Render bar plot download button only when ready
  output$downloadBarPlotUi <- renderUI({
    req(barPlotReady())
    downloadButton(
      "downloadBarPlot",
      icon = icon("cloud-download"),
      "Download",
      class = "btn-sm btn-primary ms-auto"
    )
  })
  # Handle bar plot download
  output$downloadBarPlot <- downloadHandler(
    filename = function() {
      paste0("Shiny-TaxSEA_", gsub("_(.)", "_\\U\\1", input$database_selection, perl = TRUE), "_Bar_Plot_", format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
    },
    content = function(file){
      png(file, width = 800, height = 400)
      print(barPlot())
      dev.off()
    }
  )
  
  # Render volcano plot download button only when ready
  output$downloadVolcanoPlotUi <- renderUI({
    req(volcanoPlotReady())
    downloadButton(
      "downloadVolcanoPlot",
      icon = icon("cloud-download"),
      "Download",
      class = "btn-sm btn-primary ms-auto"
    )
  })
  
  # Handle bar plot download
  output$downloadVolcanoPlot <- downloadHandler(
    filename = function() {
      paste0("Shiny-TaxSEA_", gsub("_(.)", "_\\U\\1", input$database_selection, perl = TRUE), "_Volcano_Plot_", format(Sys.time(), "%Y%m%d_%H%M%S"),".png")
    },
    content = function(file){
      png(file, width = 800, height = 400)
      print(volcanoPlot())
      dev.off()
    }
  )
  
  # Render table download button only when ready: For both table 1 and 2
  output$downloadDataTableUi <- renderUI({
    req(dataTableReady())
    downloadButton(
      "downloadTable",
      icon = icon("cloud-download"),
      "Download",
      class = "btn-sm btn-primary ms-auto"
    )
  })
  output$downloadDataTable2Ui <- renderUI({
    req(dataTable2Ready())  
    downloadButton(
      "downloadTable2",
      icon  = icon("cloud-download"),
      "Download",
      class = "btn-sm btn-primary ms-auto"
    )
  })
  # Handle table download
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste0("Shiny-TaxSEA_", gsub("_(.)", "_\\U\\1", input$database_selection, perl = TRUE), "_Results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      write.csv(taxseaResults()[[input$database_selection]], file, row.names = FALSE)
    },
  )
  
  output$heatmap <- renderPlot({
    req(heatmapPlot())

    draw(
      heatmapPlot(),
      heatmap_legend_side = "right",
      padding = unit(c(2, 30, 2, 2), "mm")
    )
    
    heatmapReady(TRUE)
  })
  
  output$downloadHeatmapUi <- renderUI({
    req(heatmapReady())
    downloadButton(
      "downloadHeatmap",
      icon  = icon("cloud-download"),
      "Download",
      class = "btn-sm btn-primary ms-auto"
    )
  })
  
  output$width_display <- renderText(taxon_sets_amount())
  
  output$downloadHeatmap <- downloadHandler(
    filename = function() {
      paste0("Shiny-ssTaxSEA_Heatmap_", 
             input$ss_database_selection, "_",
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 120)
      draw(
        heatmapPlot(),
        heatmap_legend_side = "right",
        padding = unit(c(5, 5, 5, 5), "mm")
      )
      dev.off()
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)




# d <- read.csv("C:/Users/Admin/Documents/TaxSEA_Shiny_update/Shiny-TaxSEA/count_table_HMP_2019_IBD_data.csv")
# class(d)
