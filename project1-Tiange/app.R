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
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(DT)
library(ggplot2)


#load csv data
hate <- read.csv("Hate_Crimes_by_County_and_Bias_Type__Beginning_2010.csv")
#avoid plotly issues
pdf(NULL)

#application header and title
header <-  dashboardHeader(title="NY Hate Crime", titleWidth = 300,
                           
                           dropdownMenu(type = "notifications",
                                        notificationItem(text = "3 crimes around you", 
                                                         icon = icon("users"))
                           ),
                           dropdownMenu(type = "tasks", badgeStatus = "success",
                                        taskItem(value = 80, color = "red",
                                                 "Midichlorians")
                           ),
                           dropdownMenu(type = "messages",
                                        messageItem(
                                            from = "NYPD",
                                            message = HTML("Look at the community around you!"),
                                            icon = icon("exclamation-circle"))
                           )
)

#dashboard sidebar
sidebar<-dashboardSidebar(
    sidebarMenu(
        id = "tabs",
        
        # Menu Items ----------------------------------------------
        menuItem("summary",icon=icon("list-alt"),tabName = "summary"),
        menuItem("Hate relation", icon = icon("bar-chart"), tabName = "scatter_plot"),
        menuItem("Victims", icon = icon("bar-chart"), tabName = "box_type"),
        menuItem("Table", icon = icon("th"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
        
        # Inputs: select county to plot ----------------------------------------------
        selectInput(inputId = "county",
                    label = "Select County",
                    choices = sort(unique(hate$County)),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = "Albany"),
        
        # Inputs: select quarter to plot ----------------------------------------------
        checkboxGroupInput(inputId = "type",
                           label = "Select Crime Type",
                           choices = sort(unique(hate$Crime.Type)),
                           selected = c("Crimes Against Persons","Property Crimes")),
        # Reporting year Selection ----------------------------------------------
        sliderInput(inputId = "year",
                    label = "Year Range",
                    min = min(hate$Year),
                    max = max(hate$Year),
                    value = c(min(hate$Year), max(hate$Year)),
                    step = 1,
                    sep = ""),
        numericInput(inputId="size",
                     label="point size in scatterplot",
                     value=1.5,min=1,max=3,step=0.1),
        
        #select y-axis and x-axis of the scatterplot
        selectInput(inputId="y",
                    label="Y-aixs of scatterplot",
                    choices=sort(unique(colnames(hate))),
                    selected="Anti-White"),
        
        selectInput(inputId="x",
                    label="Y-aixs of scatterplot",
                    choices=sort(unique(colnames(hate))),
                    selected="Anti-Black"),
        
        #ask user to give a title to scatterplot
        textInput(inputId="plot_title",label="Plot title of scatterplot",placeholder="Enter plot title you want")
        )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
    #summary data page
    tabItem("summary",
            fluidRow(
                valueBoxOutput("victims"),
                valueBoxOutput("offenders")
            )),
    
    tabItem("scatter_plot",
            
            fluidRow(
                tabBox(title="scatterplot of all parks in the selected county",
                       width=12,
                       tabPanel("scatter",plotlyOutput("scatter_plot"))
                )
                
            )),
    
    tabItem("box_type",
            # Plot ----------------------------------------------
            fluidRow(
                tabBox(width = 8,
                       tabPanel("victims by crime Type", plotlyOutput("box_type")))
            )),
    
    tabItem("table",
            fluidPage(
                box(title = "Statisical Report for Your Selection", DT::dataTableOutput("table"), width = 12)))
    
)
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

# Define server function required to create plots and value boxes -----
server <- function(input, output) {
    # Reactive data function -------------------------------------------
   hateInput <- reactive({
        hate <- hate %>%
            
            #Year Filter
            filter(Year >= input$year[1] & Year <= input$year[2])
        
        #County Filter
        hate <- subset(hate, County %in% input$county)
        
        #Quarter Filter
        hate <- subset(hate, Crime.Type %in% input$type)
        
        # Return dataframe ----------------------------------------------
        return(hate)
    })
   
   customerizedtitle<-reactive({(input$plot_title)})
   #vitims mean value box
   output$victims<-renderValueBox({
       hi<-hateInput()
       mean_vic<-round(mean(hi$TotalVictims,na.rm=T),2)
       valueBox("Avg Victims",value=mean_vic,subtitle=paste("average victims of",nrow(hi), "counties in NY state"),icon=icon("sort-numeric-asc"),color="fuchsia")
   })
   #offenders mean value box
   output$offenders<-renderValueBox({
       hi<-hateInput()
       mean_off<-round(mean(hi$TotalOffenders,na.rm=T),2)
       valueBox("Avg Offenders",value=mean_off,subtitle=paste("average offenders of",nrow(hi), "counties in NY state"),icon=icon("sort-numeric-asc"),color="purple")
   })
   #Scatter plot
   output$scatter_plot<-renderPlotly({
       ggplotly(ggplot(data=hateInput(),aes_string(x=input$x,y=input$y,color=input$z))+
                    geom_point()+
                    labs(title=customerizedtitle())+geom_smooth()+theme_bw()+scale_shape(),
                tooltip=c('x','y'))
       
   })
   # A boxplot showing 
   output$box_type <- renderPlotly({
       ggplot(data = hateInput(),
              aes(x = Crime.Type,
                  y = Total.Victims)) +
           geom_boxplot(aes(fill = Crime.Type)) + 
           labs(fill="Type" , x = "Crime Type",  y = "The number of victims") +
           scale_fill_brewer(palette = "Blues") + 
           theme_classic()
   })
   # Data table of characters ----------------------------------------------
   output$table <- DT::renderDataTable({
       ha <-subset(hateInput(), select = c(Year, Total.Incidents, Total.Victims, Total.Offenders, County))
       DT::datatable(ha,options = list())
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)