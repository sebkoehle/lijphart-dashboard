library(shiny)
library(ggplot2)
library(ggrepel)

# Define UI
ui <- fluidPage(
  titlePanel("Lijphart Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Dataset Selection"),
      radioButtons("dataset", 
                   "Choose dataset:",
                   choices = c("Original (1945-1996)" = "dataset1",
                             "Extended (1945-2010 & 1981-2010)" = "dataset2"),
                   selected = "dataset1"),
      
      hr(),
      
      h4("Plot Selection"),
      selectInput("plot_choice",
                  "Choose plot to display:",
                  choices = c("Scatter Plots" = "scatter_header",
                            "Plot 1: FEDUN vs EXPART" = "plot1",
                            "Plot 2: MINWIN vs EFFNUMPA" = "plot2",
                            "Plot 3: MINWIN vs EXDOM" = "plot3",
                            "Plot 4: DISPROP vs EFFNUMPA" = "plot4",
                            "Plot 5: MINWIN vs PLURALIS" = "plot5",
                            "Plot 6: EFFNUMPA vs PLURALIS" = "plot6",
                            "Plot 7: FEDERALI vs BICAMERA" = "plot7",
                            "Plot 8: CONSTRIG vs JUDREVIE" = "plot8",
                            "Plot 9: FEDERALI vs CENTRALB" = "plot9",
                            "Dotplots" = "dotplot_header",
                            "Dotplot: EXPART" = "dot_expart",
                            "Dotplot: FEDUN" = "dot_fedun",
                            "Dotplot: EFFNUMPA" = "dot_effnumpa",
                            "Dotplot: MINWIN" = "dot_minwin",
                            "Dotplot: EXDOM" = "dot_exdom",
                            "Dotplot: DISPROP" = "dot_disprop",
                            "Dotplot: PLURALIS" = "dot_pluralis",
                            "Dotplot: BICAMERA" = "dot_bicamera",
                            "Dotplot: FEDERALI" = "dot_federali",
                            "Dotplot: JUDREVIE" = "dot_judrevie",
                            "Dotplot: CONSTRIG" = "dot_constrig",
                            "Dotplot: CENTRALB" = "dot_centralb"),
                  selected = "plot1"),
      
      hr(),
      
      h4("Plot Options"),
      checkboxInput("add_regression", "Add regression line", value = FALSE),
      
      hr(),
      
      helpText("EXPART: Executive-Parties Dimension",
               br(),
               "FEDUN: Federal-Unitary Dimension",
               br(),
               "EFFNUMPA: Effective Number of Parliamentary Parties",
               br(),
               "MINWIN: Minimal Winning One-Party Cabinets (%)",
               br(),
               "EXDOM: Executive Dominance",
               br(),
               "DISPROP: Disproportionality",
               br(),
               "PLURALIS: Interest Group Pluralism",
               br(),
               "BICAMERA: Bicameralism",
               br(),
               "FEDERALI: Federalism",
               br(),
               "JUDREVIE: Judicial Review",
               br(),
               "CONSTRIG: Constitutional Rigidity",
               br(),
               "CENTRALB: Central Bank Independence")
    ),
    
    mainPanel(
      plotOutput("selectedPlot", height = "600px"),
      hr(),
      h4("Summary Statistics"),
      verbatimTextOutput("summary")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load the datasets from the same directory as the app
  data1 <- read.csv("lijphart.csv")
  data2 <- read.csv("Lijphart_AE.csv")
  
  # Reactive expression to select the appropriate dataset
  selected_data <- reactive({
    if (input$dataset == "dataset1") {
      data1
    } else {
      # For dataset2, we'll use the 1945-2010 period variables
      data.frame(
        COUNTRY = data2$country,
        EXPART = data2$exec_parties_1945_2010,
        FEDUN = data2$federal_unitary_1945_2010,
        EFFNUMPA = data2$eff_num_parl_parties_1945_2010,
        MINWIN = data2$pct_minimal_winning_one_party_cabinet_1945_2010,
        EXDOM = data2$index_of_exec_dominance_1945_2010,
        DISPROP = data2$index_of_disproportionality_1945_2010,
        PLURALIS = data2$index_of_interest_group_pluralism_1945_2010,
        BICAMERA = data2$index_of_bicameralism_1945_2010,
        FEDERALI = data2$index_of_federalism_1945_2010,
        JUDREVIE = data2$index_of_judicial_review_1945_2010,
        CONSTRIG = data2$index_of_const_rigidity_1945_2010,
        CENTRALB = data2$index_of_central_bank_independence_1945_1994
      )
    }
  })
  
  # Create the selected plot based on user choice
  output$selectedPlot <- renderPlot({
    data <- selected_data()
    
    # Determine which plot to show
    if (input$plot_choice == "plot1") {
      # Plot 1: FEDUN vs EXPART
      p <- ggplot(data, aes(x = EXPART, y = FEDUN)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
        geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
        labs(
          title = "Federal-Unitary vs Executive-Parties Dimension",
          x = "Executive-Parties Dimension (EXPART)",
          y = "Federal-Unitary Dimension (FEDUN)"
        )
    } else if (input$plot_choice == "plot2") {
      # Plot 2: MINWIN vs EFFNUMPA
      p <- ggplot(data, aes(x = EFFNUMPA, y = MINWIN)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Minimal Winning Cabinets vs Effective Number of Parties",
          x = "Effective Number of Parliamentary Parties (EFFNUMPA)",
          y = "Minimal Winning One-Party Cabinets (%)"
        )
    } else if (input$plot_choice == "plot3") {
      # Plot 3: MINWIN vs EXDOM
      p <- ggplot(data, aes(x = EXDOM, y = MINWIN)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Minimal Winning Cabinets vs Executive Dominance",
          x = "Executive Dominance (EXDOM)",
          y = "Minimal Winning One-Party Cabinets (%)"
        )
    } else if (input$plot_choice == "plot4") {
      # Plot 4: DISPROP vs EFFNUMPA
      p <- ggplot(data, aes(x = EFFNUMPA, y = DISPROP)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Disproportionality vs Effective Number of Parties",
          x = "Effective Number of Parliamentary Parties (EFFNUMPA)",
          y = "Disproportionality (DISPROP)"
        )
    } else if (input$plot_choice == "plot5") {
      # Plot 5: MINWIN vs PLURALIS
      p <- ggplot(data, aes(x = PLURALIS, y = MINWIN)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Minimal Winning Cabinets vs Interest Group Pluralism",
          x = "Interest Group Pluralism (PLURALIS)",
          y = "Minimal Winning One-Party Cabinets (%)"
        )
    } else if (input$plot_choice == "plot6") {
      # Plot 6: EFFNUMPA vs PLURALIS
      p <- ggplot(data, aes(x = PLURALIS, y = EFFNUMPA)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Effective Number of Parties vs Interest Group Pluralism",
          x = "Interest Group Pluralism (PLURALIS)",
          y = "Effective Number of Parliamentary Parties (EFFNUMPA)"
        )
    } else if (input$plot_choice == "plot7") {
      # Plot 7: FEDERALI vs BICAMERA
      p <- ggplot(data, aes(x = BICAMERA, y = FEDERALI)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Federalism vs Bicameralism",
          x = "Bicameralism (BICAMERA)",
          y = "Federalism (FEDERALI)"
        )
    } else if (input$plot_choice == "plot8") {
      # Plot 8: CONSTRIG vs JUDREVIE
      p <- ggplot(data, aes(x = JUDREVIE, y = CONSTRIG)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.8,
                        point.padding = 0.5,
                        min.segment.length = 0,
                        force = 2,
                        force_pull = 0.5) +
        labs(
          title = "Constitutional Rigidity vs Judicial Review",
          x = "Judicial Review (JUDREVIE)",
          y = "Constitutional Rigidity (CONSTRIG)"
        )
    } else if (input$plot_choice == "plot9") {
      # Plot 9: FEDERALI vs CENTRALB
      p <- ggplot(data, aes(x = CENTRALB, y = FEDERALI)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_text_repel(aes(label = COUNTRY), 
                        size = 3,
                        max.overlaps = Inf,
                        box.padding = 0.5,
                        point.padding = 0.3) +
        labs(
          title = "Federalism vs Central Bank Independence",
          x = "Central Bank Independence (CENTRALB)",
          y = "Federalism (FEDERALI)"
        )
    } else if (grepl("^dot_", input$plot_choice)) {
      # Dotplots for individual variables
      var_name <- toupper(sub("dot_", "", input$plot_choice))
      
      # Get the variable data
      if (var_name == "EXPART") {
        var_data <- data$EXPART
        var_label <- "Executive-Parties Dimension (EXPART)"
      } else if (var_name == "FEDUN") {
        var_data <- data$FEDUN
        var_label <- "Federal-Unitary Dimension (FEDUN)"
      } else if (var_name == "EFFNUMPA") {
        var_data <- data$EFFNUMPA
        var_label <- "Effective Number of Parliamentary Parties (EFFNUMPA)"
      } else if (var_name == "MINWIN") {
        var_data <- data$MINWIN
        var_label <- "Minimal Winning One-Party Cabinets (MINWIN)"
      } else if (var_name == "EXDOM") {
        var_data <- data$EXDOM
        var_label <- "Executive Dominance (EXDOM)"
      } else if (var_name == "DISPROP") {
        var_data <- data$DISPROP
        var_label <- "Disproportionality (DISPROP)"
      } else if (var_name == "PLURALIS") {
        var_data <- data$PLURALIS
        var_label <- "Interest Group Pluralism (PLURALIS)"
      } else if (var_name == "BICAMERA") {
        var_data <- data$BICAMERA
        var_label <- "Bicameralism (BICAMERA)"
      } else if (var_name == "FEDERALI") {
        var_data <- data$FEDERALI
        var_label <- "Federalism (FEDERALI)"
      } else if (var_name == "JUDREVIE") {
        var_data <- data$JUDREVIE
        var_label <- "Judicial Review (JUDREVIE)"
      } else if (var_name == "CONSTRIG") {
        var_data <- data$CONSTRIG
        var_label <- "Constitutional Rigidity (CONSTRIG)"
      } else if (var_name == "CENTRALB") {
        var_data <- data$CENTRALB
        var_label <- "Central Bank Independence (CENTRALB)"
      }
      
      # Create dotplot data frame
      dot_data <- data.frame(
        COUNTRY = data$COUNTRY,
        VALUE = var_data
      )
      
      # Sort by value for better visualization
      dot_data <- dot_data[order(dot_data$VALUE), ]
      dot_data$COUNTRY <- factor(dot_data$COUNTRY, levels = dot_data$COUNTRY)
      
      # Create dotplot
      p <- ggplot(dot_data, aes(x = VALUE, y = COUNTRY)) +
        geom_point(color = "#440154", size = 3, alpha = 0.7) +
        geom_segment(aes(x = 0, xend = VALUE, y = COUNTRY, yend = COUNTRY),
                     color = "#440154", alpha = 0.3) +
        labs(
          title = paste("Dotplot:", var_label),
          x = var_label,
          y = "Country"
        )
    }
    
    # Apply common theme
    if (exists("p")) {
      p <- p + 
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_line(color = "gray95")
        )
    }
    
    # Add regression line if selected (only for scatter plots, not dotplots)
    if (input$add_regression && !grepl("^dot_", input$plot_choice)) {
      p <- p + geom_smooth(method = "lm", 
                          se = TRUE, 
                          color = "#21918c", 
                          linetype = "dashed",
                          alpha = 0.2)
    }
    
    p
  })
  
  # Summary statistics
  output$summary <- renderPrint({
    data <- selected_data()
    
    cat("Dataset:", ifelse(input$dataset == "dataset1", 
                          "Original (1945-1996)", 
                          "Extended (1945-2010)"), "\n\n")
    
    cat("Number of observations:", nrow(data), "\n\n")
    
    # Show statistics based on selected plot
    if (input$plot_choice == "plot1") {
      cat("=== Plot 1: FEDUN vs EXPART ===\n")
      cat("EXPART (Executive-Parties Dimension):\n")
      print(summary(data$EXPART))
      cat("\nFEDUN (Federal-Unitary Dimension):\n")
      print(summary(data$FEDUN))
      cat("\nCorrelation:", 
          round(cor(data$EXPART, data$FEDUN, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot2") {
      cat("=== Plot 2: MINWIN vs EFFNUMPA ===\n")
      cat("EFFNUMPA (Effective Number of Parliamentary Parties):\n")
      print(summary(data$EFFNUMPA))
      cat("\nMINWIN (Minimal Winning One-Party Cabinets %):\n")
      print(summary(data$MINWIN))
      cat("\nCorrelation:", 
          round(cor(data$EFFNUMPA, data$MINWIN, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot3") {
      cat("=== Plot 3: MINWIN vs EXDOM ===\n")
      cat("EXDOM (Executive Dominance):\n")
      print(summary(data$EXDOM))
      cat("\nMINWIN (Minimal Winning One-Party Cabinets %):\n")
      print(summary(data$MINWIN))
      cat("\nCorrelation:", 
          round(cor(data$EXDOM, data$MINWIN, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot4") {
      cat("=== Plot 4: DISPROP vs EFFNUMPA ===\n")
      cat("EFFNUMPA (Effective Number of Parliamentary Parties):\n")
      print(summary(data$EFFNUMPA))
      cat("\nDISPROP (Disproportionality):\n")
      print(summary(data$DISPROP))
      cat("\nCorrelation:", 
          round(cor(data$EFFNUMPA, data$DISPROP, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot5") {
      cat("=== Plot 5: MINWIN vs PLURALIS ===\n")
      cat("PLURALIS (Interest Group Pluralism):\n")
      print(summary(data$PLURALIS))
      cat("\nMINWIN (Minimal Winning One-Party Cabinets %):\n")
      print(summary(data$MINWIN))
      cat("\nCorrelation:", 
          round(cor(data$PLURALIS, data$MINWIN, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot6") {
      cat("=== Plot 6: EFFNUMPA vs PLURALIS ===\n")
      cat("PLURALIS (Interest Group Pluralism):\n")
      print(summary(data$PLURALIS))
      cat("\nEFFNUMPA (Effective Number of Parliamentary Parties):\n")
      print(summary(data$EFFNUMPA))
      cat("\nCorrelation:", 
          round(cor(data$PLURALIS, data$EFFNUMPA, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot7") {
      cat("=== Plot 7: FEDERALI vs BICAMERA ===\n")
      cat("BICAMERA (Bicameralism):\n")
      print(summary(data$BICAMERA))
      cat("\nFEDERALI (Federalism):\n")
      print(summary(data$FEDERALI))
      cat("\nCorrelation:", 
          round(cor(data$BICAMERA, data$FEDERALI, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot8") {
      cat("=== Plot 8: CONSTRIG vs JUDREVIE ===\n")
      cat("JUDREVIE (Judicial Review):\n")
      print(summary(data$JUDREVIE))
      cat("\nCONSTRIG (Constitutional Rigidity):\n")
      print(summary(data$CONSTRIG))
      cat("\nCorrelation:", 
          round(cor(data$JUDREVIE, data$CONSTRIG, use = "complete.obs"), 3))
    } else if (input$plot_choice == "plot9") {
      cat("=== Plot 9: FEDERALI vs CENTRALB ===\n")
      cat("CENTRALB (Central Bank Independence):\n")
      print(summary(data$CENTRALB))
      cat("\nFEDERALI (Federalism):\n")
      print(summary(data$FEDERALI))
      cat("\nCorrelation:", 
          round(cor(data$CENTRALB, data$FEDERALI, use = "complete.obs"), 3))
    } else if (grepl("^dot_", input$plot_choice)) {
      # Summary for dotplots
      var_name <- toupper(sub("dot_", "", input$plot_choice))
      
      if (var_name == "EXPART") {
        cat("=== Dotplot: EXPART ===\n")
        cat("Executive-Parties Dimension:\n")
        print(summary(data$EXPART))
      } else if (var_name == "FEDUN") {
        cat("=== Dotplot: FEDUN ===\n")
        cat("Federal-Unitary Dimension:\n")
        print(summary(data$FEDUN))
      } else if (var_name == "EFFNUMPA") {
        cat("=== Dotplot: EFFNUMPA ===\n")
        cat("Effective Number of Parliamentary Parties:\n")
        print(summary(data$EFFNUMPA))
      } else if (var_name == "MINWIN") {
        cat("=== Dotplot: MINWIN ===\n")
        cat("Minimal Winning One-Party Cabinets (%):\n")
        print(summary(data$MINWIN))
      } else if (var_name == "EXDOM") {
        cat("=== Dotplot: EXDOM ===\n")
        cat("Executive Dominance:\n")
        print(summary(data$EXDOM))
      } else if (var_name == "DISPROP") {
        cat("=== Dotplot: DISPROP ===\n")
        cat("Disproportionality:\n")
        print(summary(data$DISPROP))
      } else if (var_name == "PLURALIS") {
        cat("=== Dotplot: PLURALIS ===\n")
        cat("Interest Group Pluralism:\n")
        print(summary(data$PLURALIS))
      } else if (var_name == "BICAMERA") {
        cat("=== Dotplot: BICAMERA ===\n")
        cat("Bicameralism:\n")
        print(summary(data$BICAMERA))
      } else if (var_name == "FEDERALI") {
        cat("=== Dotplot: FEDERALI ===\n")
        cat("Federalism:\n")
        print(summary(data$FEDERALI))
      } else if (var_name == "JUDREVIE") {
        cat("=== Dotplot: JUDREVIE ===\n")
        cat("Judicial Review:\n")
        print(summary(data$JUDREVIE))
      } else if (var_name == "CONSTRIG") {
        cat("=== Dotplot: CONSTRIG ===\n")
        cat("Constitutional Rigidity:\n")
        print(summary(data$CONSTRIG))
      } else if (var_name == "CENTRALB") {
        cat("=== Dotplot: CENTRALB ===\n")
        cat("Central Bank Independence:\n")
        print(summary(data$CENTRALB))
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
