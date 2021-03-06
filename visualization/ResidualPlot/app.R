library(shiny)
library(ggplot2)
library(datasets)
library(tidyverse)
library(car)

# Organize datasets
statenames <- state.name[-c(2,11)]

# Load all state files  
data <- read.csv("./Data/all_state.csv")[, -1]

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
  headerPanel(title = "Residual Plots"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      selectInput("energytype", "Select an Energy Type:", 
                  choices = c("Electricity", "Natural Gas")),
      hr(), 
      selectInput("year", "Select a Year:", 
                   choices = 1990:2017),
      hr(), 
      selectInput("state", "Select a State",
                  choices = statenames),
      hr(),
      checkboxInput("checkbox", label = "Aggregated by Years", value = FALSE),
      checkboxInput("lines", label = "Change Points to Lines", value = FALSE),
      checkboxInput("logtrans", label= "Log Transformation", value = FALSE),
      
      hr(), 
      helpText("The graph on the right shows residuals from annual plots")
    ),
    
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Two types of ggplots
      tabsetPanel(
        id = "figures",
        tabPanel("Residual Plot with DD", 
                 plotOutput("ResDD", 
                            hover = hoverOpts(id = "plot_hover", delay=10, delayType = "debounce"))),
        tabPanel("Residual Plot with DD + GDP", 
                 plotOutput("ResDDGDP")),
        tabPanel("Residual Plots", 
                 plotOutput("Residual"))
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
  
  output$ResDDGDP <- renderPlot({
    
    # define a table to capture residuals
    df <- as.data.frame(matrix(ncol=0, nrow=12))
    df_total <- as.data.frame(matrix(ncol=3, nrow=0))
    
    dataset <- data %>%
      filter(State == input$state)
    
    # edit date
    dataset$Date<-format(seq(as.Date("1990-01-15"), as.Date("2017-12-15"), by="1 month"))
    dataset$Date<-as.Date(dataset$Date)      
    
    # creating a loop for all years till the year input (included)
    if (input$checkbox){
      for (i in 1990:input$year){
        statedata <- dataset %>%
          filter(Year == i) %>%
          select(Date, input$energytype, DD, GDPpc)
        
        colnames(statedata) <- c("Date", "Energy", "DD", "GDPpc")
        
       
        # give an option to do natural logarithm 
        if (input$logtrans){
          statedata$Energy <- log(statedata$Energy)}
        
          lm_fit <- lm(Energy ~ DD + I(DD^2) + GDPpc, statedata)
          
          df[, "Residual"] <- resid(lm_fit)
          df[, "DD"] <- statedata$DD
          df[, "Year"] <- rep(i, 12)
          
        
        df_total <- rbind(df_total, df)
        
      }
      
      df_total$Year <- as.factor(df_total$Year)
      df <- df_total
      
    } else { 
      statedata <- dataset %>%
        filter(Year == input$year) %>%
        select(Date, input$energytype, DD, GDPpc)
      
      colnames(statedata) <- c("Date", "Energy", "DD", "GDPpc")
      
      # give an option to do natural logarithm
      if (input$logtrans){
        statedata$Energy <- log(statedata$Energy)}
      
      lm_fit <- lm(Energy ~ DD + I(DD^2) + GDPpc, statedata)
      
      df[, "Residual"] <- resid(lm_fit)
      df[, "DD"] <- statedata$DD
      df[, "Year"] <- rep(input$year, 12)
      
    }
    
    # add dw test
    dwtest <- durbinWatsonTest(lm_fit$residuals) 
    dwtest <- sprintf("%.3f", dwtest)
    
      if (input$lines){
          ggplot(df, aes(x=DD, y=Residual, color=Year)) + 
          geom_line(size = 1) +
          xlab("Degree Days")+
          ylab("Energy Use Per Capita (MJ/person)")+
          # labs(title = paste0("Energy Use Per Capita for ", input$energytype, " in ", input$state))+
          labs(caption=paste0("DW test statistics = ", dwtest, " (<0.449 for autocorrelation at lag 1)")) +
          theme(text = element_text(size=16),
                axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12))
      } else {
          ggplot(df, aes(x=DD, y=Residual, color=Year)) + 
          geom_point(size = 4) +
          xlab("Degree Days")+
          ylab("Energy Use Per Capita (MJ/person)")+
          # labs(title = paste0("Energy Use Per Capita for ", input$energytype, " in ", input$state))+
          labs(caption=paste0("DW test statistics = ", dwtest, " (<0.449 for autocorrelation at lag 1)")) +
          theme(text = element_text(size=16),
                axis.text.x = element_text(size=12),
                axis.text.y = element_text(size=12))
      }
     
    
  })
  
  
  output$ResDD <- renderPlot({
    
    # define a table to capture residuals
    df <- as.data.frame(matrix(ncol=0, nrow=12))
    df_total <- as.data.frame(matrix(ncol=3, nrow=0))
    
    dataset <- data %>%
      filter(State == input$state)
    
    # edit date
    dataset$Date<-format(seq(as.Date("1990-01-15"), as.Date("2017-12-15"), by="1 month"))
    dataset$Date<-as.Date(dataset$Date)      
    
    if (input$checkbox){
      for (i in 1990:input$year){
        statedata <- dataset %>%
          filter(Year == i) %>%
          select(Date, input$energytype, DD)
        
        colnames(statedata) <- c("Date", "Energy", "DD")
        
        # give an option to do natural logarithm
        if (input$logtrans){
          statedata$Energy <- log(statedata$Energy)}
        
        
        lm_fit <- lm(Energy ~ DD + I(DD^2), statedata)
        
        df[, "Residual"] <- resid(lm_fit)
        df[, "DD"] <- statedata$DD
        df[, "Year"] <- rep(i, 12)
        
        df_total <- rbind(df_total, df)
        
      }
      
      df_total$Year <- as.factor(df_total$Year)
      df <- df_total
      
    } else { 
      statedata <- dataset %>%
        filter(Year == input$year) %>%
        select(Date, input$energytype, DD)
      
      colnames(statedata) <- c("Date", "Energy", "DD")
      
      # give an option to do natural logarithm
      if (input$logtrans){
        statedata$Energy <- log(statedata$Energy)}
      
      
      lm_fit <- lm(Energy ~ DD + I(DD^2), statedata)
      
      df[, "Residual"] <- resid(lm_fit)
      df[, "DD"] <- statedata$DD
      df[, "Year"] <- rep(input$year, 12)
      
    }
    
    # add dw test
    dwtest <- durbinWatsonTest(lm_fit$residuals) 
    dwtest <- sprintf("%.3f", dwtest)
    
    
    if (input$lines){
      ggplot(df, aes(x=DD, y=Residual, color=Year)) + 
        geom_line(size = 1) +
        xlab("Degree Days")+
        ylab("Energy Use Per Capita (MJ/person)")+
        # labs(title = paste0("Energy Use Per Capita for ", input$energytype, " in ", input$state))+
        labs(caption=paste0("DW test statistics = ", dwtest, " (<0.449 for autocorrelation at lag 1)")) +
        theme(text = element_text(size=16),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))
    } else {
      ggplot(df, aes(x=DD, y=Residual, color=Year)) + 
        geom_point(size = 4) +
        xlab("Degree Days")+
        ylab("Energy Use Per Capita (MJ/person)")+
        # labs(title = paste0("Energy Use Per Capita for ", input$energytype, " in ", input$state))+
        labs(caption=paste0("DW test statistics = ", dwtest, " (<0.449 for autocorrelation at lag 1)")) +
        theme(text = element_text(size=16),
              axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12))
    }
    
    
  })
  
  
  output$Residual <- renderPlot({
    
    # define a table to capture residuals
    df <- as.data.frame(matrix(ncol=0, nrow=12))
    df_total <- as.data.frame(matrix(ncol=3, nrow=0))
    
    dataset <- data %>%
      filter(State == input$state)
    
    # edit date
    dataset$Date<-format(seq(as.Date("1990-01-15"), as.Date("2017-12-15"), by="1 month"))
    dataset$Date<-as.Date(dataset$Date)      
    
    
    # option to include all previous years
    if (input$checkbox){
      for (i in 1990:input$year){
        statedata <- dataset %>%
          filter(Year == i) %>%
          select(Date, input$energytype, DD)
        
        colnames(statedata) <- c("Date", "Energy", "DD")
        
        # give an option to do natural logarithm
        if (input$logtrans){
          statedata$Energy <- log(statedata$Energy)}
        
        DDfit <- lm(Energy ~ DD + I(DD^2), statedata)
        
      }
    } else { 
      statedata <- dataset %>%
        filter(Year == input$year) %>%
        select(Date, input$energytype, DD)
      
      colnames(statedata) <- c("Date", "Energy", "DD")
      
      # give an option to do natural logarithm
      if (input$logtrans){
        statedata$Energy <- log(statedata$Energy)}
      
      DDfit <- lm(Energy ~ DD + I(DD^2), statedata)
    
    }
    
    bptest <- lmtest::bptest(DDfit)
    bptest <- sprintf("%.3f", bptest)
    
    
    par(mfrow=c(2,2)) # init 4 charts in 1 panel
    plot(DDfit)
    
    print(paste0("bp test statistics = ", bptest, " <0.05 to indicate heteroscedasticity"))
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
