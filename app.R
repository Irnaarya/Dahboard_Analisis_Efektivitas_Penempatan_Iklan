#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(gridExtra)


# Tentukan UI
ui <- fluidPage(
  titlePanel("Analisis Efektivitas Penempatan Iklan"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Unggah Data (format CSV)"),
      actionButton("analisis", "Analisis")
    ),
    mainPanel(
      tags$style(HTML("
        #summary_text {
          background-color: #f0f8ff;  /* Yellow background for summary_text */
        }
      ")),
      plotOutput("side_by_side_plots"),
      verbatimTextOutput("summary_text")
    )
  )
)

# Tentukan server
server <- function(input, output) {
  data <- reactive({
    if (!is.null(input$file)) {
      read.csv(input$file$datapath)
    } else {
      data.frame(
        PenempatanIklan = character(0),  # Empty character vector
        CTR = numeric(0)
      )
    }
  })
  
  output$side_by_side_plots <- renderPlot({
    req(data())
    
    if (nrow(data()) > 0) {
      # Create histogram
      hist_plot <- ggplot(data(), aes(x = CTR, fill = PenempatanIklan)) +
        geom_histogram(position = "dodge", binwidth = 0.5, color = "black", alpha = 0.7) +
        labs(title = "Histogram of CTR", x = "CTR", y = "Frequency") +
        theme_minimal()
      
      # Create boxplot
      box_plot <- ggplot(data(), aes(x = PenempatanIklan, y = CTR, fill = PenempatanIklan)) +
        geom_boxplot() +
        labs(title = "Boxplot of CTR", x = "Penempatan Iklan", y = "CTR") +
        theme_minimal()
      
      # Arrange plots side by side
      grid.arrange(hist_plot, box_plot, ncol = 2)
    }
  })
  
  output$summary_text <- renderPrint({
    req(data())  # Ensure data() is not null
    
    if (nrow(data()) > 0) {
      result <- aov(CTR ~ PenempatanIklan, data = data())
      
      cat("Hasil Analisis ANOVA:\n")
      print(result)
      
      p_value <- summary(result)[[1]][["Pr(>F)"]]
      
      if (length(unique(data()$PenempatanIklan)) == 2) {
        if (p_value < 0.05) {
          cat("\nKesimpulan: Hasilnya signifikan secara statistik pada tingkat signifikansi 0.05.")
        } else {
          cat("\nKesimpulan: Tidak ada perbedaan signifikan dalam CTR berdasarkan penempatan iklan.")
        }
      } else {
        cat("\nPerhatian: Pastikan 'PenempatanIklan' hanya memiliki dua level.")
      }
    } else {
      cat("\nPerhatian: Data tidak tersedia atau tidak sesuai format yang diharapkan.")
    }
  })
}

# Jalankan aplikasi
shinyApp(ui, server)

