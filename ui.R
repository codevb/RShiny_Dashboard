library(shiny)
require(shinydashboard)
library(ggplot2)
library(plotly)


#HEADER
header <- dashboardHeader(title = "Global Sales Overview")

#SIDEBAR
sidebar <- dashboardSidebar(sidebarMenu(id="sidebarmenu",
  menuItem("Sales Dashboard", tabName = "Sales_Dashboard", icon = icon("dashboard")),
  menuItem("Active Users",  icon = icon("user"), tabName = "Active_Users"),
  menuItem("Revenue",  icon = icon("dollar"),tabName = "Revenue"),
  menuItem("Activation Rate",  icon = icon("user"),tabName = "Activation_Rate"),
  menuItem("Success Rate",  icon = icon("thumbs-up"),tabName = "Success_Rate"),
  conditionalPanel("input.sidebarmenu === 'Active_Users'",
                   selectInput("inputYear", "Input Year",
                               choices = c("2015","2016", "2017", "2018", "2019"), multiple=FALSE, selected = "2015",
                               width = '98%')
  
  ),
  conditionalPanel("input.sidebarmenu === 'Active_Users'",
                   selectInput("inputMonth", "Input Month",
                               choices = c("1", "2", "3", "4","5","6","7","8","9","10","11","12"), multiple=FALSE, selected = "1",
                               width = '98%')
  ),
  
  conditionalPanel("input.sidebarmenu === 'Revenue'",
                   selectInput("inputYear1", "Select Year",
                               choices = c("All", "2015", "2016", "2017", "2018", "2019"), selected = "All", multiple=FALSE, selectize=TRUE,
                               width = '98%')
  ),
  conditionalPanel("input.sidebarmenu === 'Activation_Rate'",
                   selectInput("inputYear2", "Select Year",
                               choices = c(  "2016", "2017", "2018", "2019"), selected = "2016", multiple=FALSE, selectize=TRUE,
                               width = '98%')
  ),
  conditionalPanel("input.sidebarmenu === 'Success_Rate'",
                   selectInput("inputYear3", "Input Year",
                               choices = c("2015","2016", "2017", "2018", "2019"), multiple=FALSE, selected = "2015",
                               width = '98%')
                   
  ),
  conditionalPanel("input.sidebarmenu === 'Success_Rate'",
                   selectInput("inputMonth3", "Input Month",
                               choices = c("1", "2", "3", "4","5","6","7","8","9","10","11","12"), multiple=FALSE, selected = "1",
                               width = '98%')
  )
))


#MAKING FROWS FOR BODY
frow1 <- fluidRow(
  valueBoxOutput("total_amount"),
  valueBoxOutput("total_num")
 )


frow2 <- fluidRow(
  box(
    title = "Sales by Vendor"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("vendorSales", height = "300px")
  ),
  box(
    title = "Transactions Per Store"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("MaxTransactions", height = "300px")
  )
  )


frow3 <- fluidRow(
  tabBox(
    title = "Data Viewer"
    ,width = 12
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Sales by Stores"
      ,dataTableOutput("tbl")
    ),
    tabPanel(
      title = "Transactions Per Store"
      ,dataTableOutput("tbl2")
    )
  )
)
#frow1, frow2 and frow3 for sales dashboard page
frow4 <- fluidRow(
  valueBoxOutput("total_users"),
  valueBoxOutput("daily_users")
)

frow5 <- fluidRow(
  box(width = 12,
    title = "Monthly Active Users"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("ActiveUsers", height = "300px")
  ))
  
  frow10<- fluidRow(box(width = 12,
    title = "Daily Active Users"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotlyOutput("ActiveUsers1", height = "300px")
  ))





frow6 <- fluidRow(
  tabBox(
    title = "Data Viewer"
    ,width = 12
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Monthly Active Users"
      ,dataTableOutput("tbl3")
    ),
    tabPanel(
      title = "Daily Active Users"
      ,dataTableOutput("daily_tbl3")
    )
  )
)


# frow7, frow8 for revenue dashboard page
frow7 <- fluidRow(column(12, box(width = 12
                                 ,title = "Revenue By Month"
                                 ,status = "primary"
                                 ,solidHeader = TRUE 
                                 ,collapsible = TRUE 
                                 ,plotlyOutput("monthly_revenue", height = "300px")))
)

frow8 <- fluidRow(column(12, box(width = 12
                                 ,title = "Month Over Month Revenue Growth"
                                 ,status = "primary"
                                 ,solidHeader = TRUE 
                                 ,collapsible = TRUE 
                                 ,plotlyOutput("mom_revenue_growth", height = "300px")))
)

frow9 <- fluidRow(
  tabBox(
    title = "Data Viewer"
    ,width = 12
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Monthly Revenue"
      ,DT::dataTableOutput("tbl4")
    )
  )
)


#frow 11,12 for the activation page

frow11 <- fluidRow(column(12, box(width = 12
                                 ,title = "Monthly Activation Rate"
                                 ,status = "primary"
                                 ,solidHeader = TRUE 
                                 ,collapsible = TRUE 
                                 ,plotlyOutput("Activation", height = "300px")))
)

frow12 <- fluidRow(
  tabBox(
    title = "Data Viewer"
    ,width = 12
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Yearly Activation Rate"
      ,DT::dataTableOutput("act_tbl")
    )
  )
)
#frow 13,14 for the success page

 frow13 <- fluidRow(column(12, box(width = 12
                                   ,status = "primary"
                                   ,solidHeader = TRUE 
                                   ,collapsible = TRUE 
                                   ,plotlyOutput("suck", height = "300px")))
 )

frow14 <- fluidRow(
  tabBox(
    title = "Data Viewer"
    ,width = 12
    ,id = "dataTabBox"
    ,tabPanel(
      title = "Success Rate for Month and Year"
      ,DT::dataTableOutput("suc_tbl")
    )
  )
)

body<-dashboardBody(
  
  tabItems(
    tabItem(tabName = "Sales_Dashboard",
            frow1,frow2,frow3)
  ,
  tabItem(tabName = "Active_Users",
        frow4,frow5,frow10,frow6)
  ,
  tabItem(tabName = "Revenue",
          frow7, frow8, frow9)
  ,
  tabItem(tabName = "Activation_Rate",
          frow11,frow12)
  ,
  tabItem(tabName = "Success_Rate",
          frow13,frow14)
)
  
)
#body <- dashboardBody(frow1,frow2,frow3)

#DISPLAYING THE DASHBOARDPAGE
dashboardPage(header, sidebar, body)

