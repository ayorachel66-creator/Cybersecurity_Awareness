
library(shiny)
library(ggplot2)

# Load dataset
cyber_data <- read.csv("cybersecurity_awareness.csv")

# User Interface
ui <- fluidPage(
  
  tags$head(
    tags$title("Cybersecurity Awareness Dashboard"),
    tags$style(HTML("
      body {
        background-color: #10172A;
        color: #F4F6FC;
        font-family: 'Segoe UI', sans-serif;
      }

      .main-title {
        color: #20C5D9;
        font-size: 30px;
        font-weight: bold;
        padding: 20px 0 5px 0;
      }

      .subtitle {
        color: #AAB7D4;
        margin-bottom: 25px;
      }

      .sidebar {
        background-color: #19243B;
        padding: 20px;
        border-radius: 12px;
      }

      .well {
        background-color: #19243B;
        border: 1px solid #263B5D;
        color: white;
      }

      .form-control {
        background-color: #263653;
        color: white;
        border: 1px solid #20C5D9;
      }

      .stat-card {
        background: linear-gradient(135deg, #19243B, #253B62);
        border: 1px solid #20C5D9;
        border-radius: 12px;
        padding: 20px;
        margin: 8px 0 18px 0;
        text-align: center;
        min-height: 110px;
      }

      .stat-number {
        color: #20C5D9;
        font-size: 28px;
        font-weight: bold;
      }

      .stat-label {
        color: #D5DEF0;
        font-size: 13px;
      }

      .panel {
        background-color: #19243B;
        border: 1px solid #354B70;
        border-radius: 12px;
        padding: 15px;
        margin-bottom: 20px;
      }

      .panel h4 {
        color: #20C5D9;
        font-weight: bold;
      }

      .btn {
        background-color: #7955E8;
        color: white;
        border: none;
      }

      .btn:hover {
        background-color: #20C5D9;
        color: #10172A;
      }

      table {
        color: white !important;
      }

      th {
        background-color: #253B62;
      }
    "))
  ),
  
  titlePanel(
    div(
      class = "main-title",
      "CYBERSECURITY AWARENESS DASHBOARD"
    )
  ),
  
  p(
    class = "subtitle",
    "Monitoring Student Cybersecurity Knowledge and Internet Safety Practices"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      class = "sidebar",
      
      h4("DASHBOARD FILTERS"),
      
      selectInput(
        "gender",
        "Select Gender:",
        choices = c("All", sort(unique(cyber_data$Gender)))
      ),
      
      selectInput(
        "year",
        "Select Year Level:",
        choices = c("All", sort(unique(cyber_data$Year_Level)))
      ),
      
      selectInput(
        "awareness",
        "Awareness Level:",
        choices = c(
          "All",
          sort(unique(cyber_data$Cybersecurity_Awareness_Level))
        )
      ),
      
      actionButton("reset", "Reset Filters"),
      
      br(), br(),
      
      p("PROJECT INFORMATION"),
      p("Topic: Cybersecurity Awareness"),
      p("Respondents: 50 Students"),
      p("Platform: R Shiny")
    ),
    
    mainPanel(
      
      fluidRow(
        column(
          4,
          div(class = "stat-card",
              div(class = "stat-label", "TOTAL STUDENTS"),
              div(class = "stat-number", textOutput("total_students"))
          )
        ),
        
        column(
          4,
          div(class = "stat-card",
              div(class = "stat-label", "AVERAGE INTERNET HOURS"),
              div(class = "stat-number", textOutput("average_hours"))
          )
        ),
        
        column(
          4,
          div(class = "stat-card",
              div(class = "stat-label", "HIGH AWARENESS STUDENTS"),
              div(class = "stat-number", textOutput("high_awareness"))
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(class = "panel",
              h4("Cybersecurity Awareness Distribution"),
              plotOutput("awareness_plot", height = "280px")
          )
        ),
        
        column(
          6,
          div(class = "panel",
              h4("Internet Usage Hours"),
              plotOutput("internet_plot", height = "280px")
          )
        )
      ),
      
      fluidRow(
        column(
          12,
          div(class = "panel",
              h4("Two-Factor Authentication Practices"),
              plotOutput("authentication_plot", height = "300px")
          )
        )
      ),
      
      div(
        class = "panel",
        h4("STUDENT CYBERSECURITY DATA"),
        tableOutput("student_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    
    data <- cyber_data
    
    if (input$gender != "All") {
      data <- data[data$Gender == input$gender, ]
    }
    
    if (input$year != "All") {
      data <- data[data$Year_Level == input$year, ]
    }
    
    if (input$awareness != "All") {
      data <- data[
        data$Cybersecurity_Awareness_Level == input$awareness, ]
    }
    
    data
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "gender", selected = "All")
    updateSelectInput(session, "year", selected = "All")
    updateSelectInput(session, "awareness", selected = "All")
  })
  
  output$total_students <- renderText({
    nrow(filtered_data())
  })
  
  output$average_hours <- renderText({
    x <- filtered_data()$Internet_Usage_Hours
    if (length(x) == 0) return("N/A")
    paste0(round(mean(x, na.rm = TRUE), 1), " hrs")
  })
  
  output$high_awareness <- renderText({
    x <- filtered_data()$Cybersecurity_Awareness_Level
    sum(tolower(as.character(x)) == "high", na.rm = TRUE)
  })
  
  output$awareness_plot <- renderPlot({
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No data available."))
    
    ggplot(data, aes(
      x = Cybersecurity_Awareness_Level,
      fill = Cybersecurity_Awareness_Level
    )) +
      geom_bar() +
      scale_fill_manual(
        values = c(
          "Low" = "#7955E8",
          "Medium" = "#20C5D9",
          "High" = "#36D399"
        )
      ) +
      labs(x = "Awareness Level", y = "Number of Students") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#19243B"),
        panel.background = element_rect(fill = "#19243B"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "#20C5D9"),
        legend.position = "none"
      )
  })
  
  output$internet_plot <- renderPlot({
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No data available."))
    
    ggplot(data, aes(x = Internet_Usage_Hours)) +
      geom_histogram(
        bins = 10,
        fill = "#20C5D9",
        color = "#10172A"
      ) +
      labs(x = "Internet Usage Hours", y = "Frequency") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#19243B"),
        panel.background = element_rect(fill = "#19243B"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "#20C5D9")
      )
  })
  
  output$authentication_plot <- renderPlot({
    data <- filtered_data()
    
    validate(need(nrow(data) > 0, "No data available."))
    
    ggplot(data, aes(
      x = Two_Factor_Authentication,
      fill = Two_Factor_Authentication
    )) +
      geom_bar() +
      scale_fill_manual(
        values = c("#7955E8", "#20C5D9", "#36D399", "#F59E0B"),
        guide = "none"
      ) +
      labs(
        x = "Two-Factor Authentication",
        y = "Number of Students"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#19243B"),
        panel.background = element_rect(fill = "#19243B"),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        axis.title = element_text(color = "#20C5D9")
      )
  })
  
  output$student_table <- renderTable({
    head(filtered_data(), 50)
  })
  
}

# Run Shiny Application
shinyApp(ui = ui, server = server)
