library(shiny)
library(ggplot2)
library(datasets)
library(tidyverse)
library(here)

# Organize datasets
statenames <- state.name[-c(2,11)]

# Load all state files  
data <- readRDS(here("visualization", "vizdata", "all_state.rds"))

# Set column names
colnames(data)<-c(
  "Index",
  "Date", 
  "GDPpc",
  "Electricity", 
  "Natural Gas", 
  "HDD",
  "CDD",
  "DD",
  "State",
  "Year")


ui <-fluidPage(
  
  # Application title            
  headerPanel(title = "Impact of Degree Days on State Monthly Energy Use"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectInput("energytype", "Select an Energy Type", 
                  choices = c("Electricity", "Natural Gas")),
      hr(), 
      numericInput("year", "Select a Year", 
                  min = 1990, max = 2017, step = 1),
      hr(), 
      selectInput("state", "Select a State",
                  choices = statenames),
      hr(), 
      helpText("The graph on the right shows change in energy over time and at different degree days")
      ),
   
    
  # Main panel for displaying outputs
  mainPanel(
      
    # Output: Two types of ggplots
    tabsetPanel(
        id = "figures",
        tabPanel("Annual Energy Use per Capita", plotOutput("EnergyDate")),
        tabPanel("Energy Use per Capita vs. DD", plotOutput("EnergyDD"))
        ),
      
    # hr() element to introduce extra separator 
    hr(),
      
    # Name the datatable
    h4("Display data for selected state and year"),
    
    # Output: One datatable 
    DT::dataTableOutput("datatable")
      
    )
  )
)


server <- function(input, output){

  # Return the requested dataset
  
  output$EnergyDate <- renderPlot({
    
    dataset <- data %>%
      filter(State == input$state)
    
    # edit date
    dataset$Date<-format(seq(as.Date("1990-01-15"), as.Date("2017-12-15"), by="1 month"))
    dataset$Date<-as.Date(dataset$Date)      
    
    statedata <- dataset %>%
      filter(Year == input$year) %>%
      select(Date, input$energytype, DD, GDPpc)
    
    colnames(statedata) <- c("Date", "Energy", "DD", "GDPpc")
    
    # add predicted value and confidence interval
    
    lm_fit <- lm(Energy ~ DD + I(DD^2) + GDPpc, statedata)
    statedata[, "PredRE"] <- lm_fit$fitted.values
    
    conf <- predict(lm_fit, interval=("confidence"))
    
    ggplot(statedata, aes(x=Date, y=Energy, group=1, color="Observed Value")) + geom_point(size = 4)+
      geom_line(aes(x=Date, y=PredRE, color = "Regression Model"))+
      geom_ribbon(data = statedata, aes(x = Date, ymin=conf[1:12,2], ymax=conf[1:12,3]), alpha=0.3, colour="grey")+
      xlab("Date")+
      ylab("Energy Use Per Capita (MJ/person)")+
      scale_color_discrete(name="Legend") +
      # labs(title = paste0("Energy Use Per Capita for ", input$energytype, " in ", input$state))+
      # labs(caption=paste0("Least squared=", r2, " vs. ", "Fixed effects=", r22)) +
      scale_x_date(date_labels = "%b-%Y")+
      theme(text = element_text(size=16),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
  })
  
  output$EnergyDD <- renderPlot({
    energytype <- input$energytype
    
    dataset <- data %>%
      filter(Year == input$year) %>%
      filter(State == input$state) %>%
      select(DD, input$energytype)
    
    
    #qplot(x=Date,y=energytype, data = dataset)
    
    ggplot(dataset, aes(x=DD, y=dataset[,2], group=1)) +
      geom_point(size = 4)+
      xlab("Degree Days")+
      ylab("Energy Use Per Capita (MJ/person)")+
      labs(title = paste0("Energy Usee Per Capita for ", input$energytype, " in ", input$state))+
      theme(text = element_text(size=16),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
  })
  
  output$datatable <- DT::renderDataTable({
    
    dataset <- data %>%
      filter(State == input$state) %>%
      select(Date, input$energytype, DD, GDPpc)
    
    colnames(dataset) <- c("Date",
                           paste(input$energytype, "per capita (MJ/person)"),
                           "Degree Days",
                           "Real GDP (millions of dollar per capita)")
    
    DT::datatable(dataset, options = list(pageLength = 5, orderClasses = TRUE))
  })
}

shinyApp(ui, server)
