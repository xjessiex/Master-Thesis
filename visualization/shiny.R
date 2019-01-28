library(shiny)
library(ggplot2)
library(datasets)

# Organize datasets
statenames <- state.name[-c(2,11)]

# Load all state files  
data <- readRDS(here("data", "all_state.rds"))

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

data$Date <- format(data$Date, "%Y-%m", tz = "GMT")


ui <-fluidPage(
  
  # Application title            
  headerPanel(title = "Impact of Degree Days on State Monthly Energy Use"),
  sidebarLayout(
    sidebarPanel(
      selectInput("energytype", "Select an Energy Type", 
                  choices = c("Electricity", "Natural Gas")),
      numericInput("year", "Select a Year", 
                  value = 1990:2017),
      selectInput("state", "Select a State",
                  choices = statenames)
      ),
    
    mainPanel(
      tabsetPanel(
        id = "figures",
        tabPanel("Annual Energy Use per Capita", plotOutput("EnergyDate")),
        tabPanel("Energy Use per Capita vs. DD", plotOutput("EnergyDD"))
        ),
     
      h4("Display data for selected state and year")
      
      
    )
  )
)


server <- function(input, output){

  # Return the requested dataset
  
  output$EnergyDD <- renderPlot({
    energytype <- input$energytype
    
    dataset <- data %>%
      filter(Year == input$year) %>%
      filter(State == input$state) %>%
      select(Date, input$energytype)
    
    
    #qplot(x=Date,y=energytype, data = dataset)
    
    ggplot(dataset, aes(x=Date, y=dataset[,2], group=1)) +
      geom_line()+
      xlab("Date")+
      ylab("Energy Use Per Capita (MJ/person)")+
      labs(title = paste0("Energy Usee Per Capita for ", input$energytype, " in ", input$state))+
      scale_x_discrete(breaks = unique(dataset$Date[seq(1,12,4)]))+
      theme(text = element_text(size=16),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12))
    
  })
  
  
  output$datatable <- DT::renderDataTable({
    DT::datatable(dataset, options = list(pageLength = 5, orderClasses = TRUE))
  })
}

shinyApp(ui, server)
