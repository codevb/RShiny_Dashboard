library(shiny)
library(ggplot2)
library(DBI)
library(pool)
library(dplyr)
library(dbplyr)
library(lubridate)
library(scales)
library(padr)
library(DT)
library(plotly)
library(ggforce)

function(input, output, session) {
  conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "shinyappsdev",
    host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
    username = "keyo",
    password = "j9bA&bAWbGl2")

  
###############################################  ##########################################
######################     DASHBOARD PAGE1   #############################################
rs<- dbSendQuery(conn, "select a.vendor_id,b.display_name,sum(a.total_amount) as 'Total_Sales' from transactions a,vendors b where a.vendor_id=b.id group by vendor_id  ;")
   # output$tbl <- renderDataTable({on.exit(dbDisconnect(conn), add = TRUE)
   #                             dbFetch(rs)})
   #  
ab<-dbFetch(rs)


rs1<-dbSendQuery(conn, "select sum(total_amount) from transactions ;") 
tot.amount<-dbFetch(rs1)
#dataset containing vendor table joined with transaction table


#output to the table view
output$tbl <- renderDataTable(ab)
   
 output$total_amount <- renderValueBox({
  valueBox(
    formatC(tot.amount, format="d", big.mark=',')
    ,"Total Transaction Amount"
    ,color = "green")
  

})

#output to the plot
  output$vendorSales <- renderPlot({
    ggplot(data = ab,
           aes(x=factor(display_name), y=Total_Sales)) +
      geom_bar(position = "dodge", stat="identity") + ylab("Sales (in Dollars)") +
      xlab("Vendor") + theme(legend.position="bottom"
                              ,plot.title = element_text(size=15, face="bold")) +
      ggtitle("Current Vendor Sales")
  })

#to find data set for transaction count by each vendor  
  
  rs2<- dbSendQuery(conn, "select a.vendor_id,b.display_name,count(a.id) as 'Total_Transaction_Count' from transactions a,vendors b where a.vendor_id=b.id group by vendor_id;")
  data2<-dbFetch(rs2)
  
  
  rs2<-dbSendQuery(conn, "select count(id) from transactions ;") 
  tot.trans<-dbFetch(rs2)
  #dataset containing vendor table joined with transaction table
  
  
  #output to the table view
  #output$tbl2 <- renderDataTable(data2)
  output$tbl2 <- renderDataTable(data2)
  
  output$total_num <- renderValueBox({
    valueBox(
      formatC(tot.trans, format="d", big.mark=',')
      ,"Number of transactions"
      ,color = "red")
    
    
  })
  
  #output to the plot
  output$MaxTransactions <- renderPlot({
    ggplot(data = data2,
           aes(x=factor(display_name), y=Total_Transaction_Count)) +
      geom_bar(position = "dodge", stat="identity") + ylab("Number Of Transactions") +
      xlab("Vendor") + theme(legend.position="bottom"
                             ,plot.title = element_text(size=15, face="bold")) +
      ggtitle("Transactions per Store")
  })
  
###################################################################################
######################### DASHBOARD PAGE 2-MONTHLY ACTIVE USERS############################
  
  
  rs3<-function(){
    conn1 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinyappsdev",
      host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
      username = "keyo",
      password = "j9bA&bAWbGl2")
    myQuery<-paste0("select distinct count(user_id) from transactions where YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear)
    tot.users<-as.numeric(fetch(dbSendQuery(conn1,myQuery)))
    dbDisconnect(conn1)
    tot.users
  }
  tot.users<-reactive({rs3()})
  
  rs6<-function(){
    conn2 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinyappsdev",
      host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
      username = "keyo",
      password = "j9bA&bAWbGl2")
    
    myQuery<-paste0("select DISTINCT MONTH(STR_TO_DATE(`transaction_date`, '%m/%d/%Y')) as 'Months', count( DISTINCT user_id) as 'Active_Users' from transactions where YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear," GROUP BY MONTH(STR_TO_DATE(`transaction_date`, '%m/%d/%Y'))")
    jan<-(fetch(dbSendQuery(conn2,myQuery)))
    dbDisconnect(conn2)
    jan
  }
  main<-reactive({rs6()})
  
  output$tbl3 <- renderDataTable(main())

    
  output$total_users <- renderValueBox({
    valueBox(
      formatC(tot.users(), format="d", big.mark=',')
      ,"TOTAL ACTIVE USERS FOR YEAR SELECTED"
      ,color = "maroon",icon = shiny::icon("calendar"))
      })
  
  #output to the plot
  output$ActiveUsers <- renderPlotly({
    ggp<-ggplot(main(), aes(x=Months, y=Active_Users, group=1)) +
      geom_point(stat='summary', fun.y=sum,size=2,color="maroon") +
      stat_summary(fun.y=sum, geom="line",size=1.25,color="maroon")+
      scale_colour_manual(values="#E1B378")+ scale_x_discrete(limits = month.abb)
    
    ggplotly(ggp,tooltip = c("Active_Users"))
  })

  
  
  
 
  
  ###################################################################################
  ######################### DASHBOARD PAGE 2-DAILYYY   ACTIVE USERS############################
  
 
  
  ### COUNTING TOTAL ACTIVE USERS 
  daily<-function(){
    conn1 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinyappsdev",
      host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
      username = "keyo",
      password = "j9bA&bAWbGl2")
    myQuery<-paste0("select count(distinct user_id) from transactions where MONTH(STR_TO_DATE(`transaction_date`, '%m/%d/%Y'))=",input$inputMonth," and YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear)
    daily.users<-as.numeric(fetch(dbSendQuery(conn1,myQuery)))
    dbDisconnect(conn1)
    daily.users
  }
  daily.users<-reactive({daily()})
  
 ############################FOR VALUE BOX AT THE TOP############################################################## 
  output$daily_users <- renderValueBox({
    valueBox(
      formatC(daily.users(), format="d", big.mark=',')
      ,"TOTAL ACTIVE USERS FOR SELECTED MONTH & YEAR"
      ,color = "blue")
  })
  
  #####################FOR THE DATA TABLE#########################################################
  daily_table<-function(){
    conn2 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "shinyappsdev",
      host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
      username = "keyo",
      password = "j9bA&bAWbGl2")
    
    myQuery<-paste0("select DISTINCT DAY(STR_TO_DATE(`transaction_date`, '%m/%d/%Y')) as 'Days', count( DISTINCT user_id) as 'Active_Users' from transactions where YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear," and MONTH(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputMonth," GROUP BY DAY(STR_TO_DATE(`transaction_date`, '%m/%d/%Y')) ")
    dailyTable<-(fetch(dbSendQuery(conn2,myQuery)))
    dbDisconnect(conn2)
    dailyTable
  }
  dailyTable<-reactive({daily_table()})
  
  output$daily_tbl3 <- renderDataTable(dailyTable())
  
  #output to the plot
  output$ActiveUsers1 <- renderPlotly({
    gg1<-ggplot(data=dailyTable(), aes(x=Days, y=Active_Users, group=1)) +
      geom_point(stat='summary', fun.y=sum,size=2,color="blue") +
      stat_summary(fun.y=sum, geom="line",size=1.25,color="blue")+
      scale_colour_manual(values="#E1B378")
    
    ggplotly(gg1,tooltip = c("Active_Users"))
    
  }) 
  
  
  
  
  ###################################################################################
  ######################### DASHBOARD PAGE 3-REVENUE############################
  
  rs4<-dbSendQuery(conn, "SELECT transaction_date as date, total_amount FROM transactions WHERE status_notes != 'CARD DECLINE';") 
  t<-dbFetch(rs4)
  
  #convert data types
  t$date <- as.Date(t$date, format="%m/%d/%y")
  t$total_amount <- as.numeric(t$total_amount)
  
  # create a column of revenue in dollars 
  t$revenue <- t$total_amount * 0.01 * 0.01
  t$revenue <- round(t$revenue, 2)
  
  # create a data frame of monthly revenue with padding missing date
  bm <- aggregate(revenue~ year(date) + month(date), data=t, FUN=sum)
  bm$date <- as.Date(paste(bm$month,"01", bm$year,sep = "-"),format = "%m-%d-%Y" )
  colnames(bm) <- c("year", "month", "revenue", "date")
  bm <- bm[ order(bm[,4]), ]
  bm <- data.frame(bm$date, bm$year, bm$month, bm$revenue)
  colnames(bm) <- c("date", "year", "month", "revenue")
  bm <- pad(bm)
  bm$year <- year(bm$date)
  bm$month <- month(bm$date)
  bm$revenue[is.na(bm$revenue)] <- 0
  
  # calculate mom revenue growth (%)
  curr <- bm$revenue[-1]
  prev <- bm$revenue[1:(length(bm$revenue)-1)]
  growth <- 100 * round( (curr-prev) / prev, 2 )
  
  
  # create a data frame of mom growth 
  g <- data.frame(bm$date[2:50],growth)
  #g <- rbind(list("2015-01-01",0), g)
  colnames(g) <- c("date", "growth")
  g$growth[is.infinite(g$growth)] <- 0 
  g$growth[is.na(g$growth)] <- 0 
  
  
  #########################Output to the plot for monthly revenue############################ 
  
  output$monthly_revenue <- renderPlotly({
    if(input$inputYear1=="All"){
      gg <- ggplot(data = bm, aes(x=date, y=revenue, text = paste("$", revenue), group=1)) + 
        geom_line(color="darkblue") + geom_point(color="darkblue") +
        scale_x_date(labels = date_format("%Y"), date_breaks = "1 year") + 
        scale_y_continuous(labels = scales::dollar) +
        labs(x="Year", y="Revenue")
      
      ggplotly(gg, tooltip = c("text"))
    }
    else{
      min <- paste(input$inputYear1,"1-1",sep="-")
      max <- paste(input$inputYear1,"12-1",sep="-")
      
      gg <- ggplot(data = subset(bm, year==input$inputYear1), aes(x=date, y=revenue, text = paste("$", revenue), group=1)) + 
        geom_line(color="darkblue", size=1.25) + geom_point(color="darkblue", size=2) +
        scale_x_date(labels = date_format("%b"), date_breaks = "1 month", limits = as.Date(c(min,max))) + 
        scale_y_continuous(labels = scales::dollar) +
        labs(x="Month", y="Revenue")
      
      ggplotly(gg, tooltip = c("text"))
    }
  })
  
  
  ######################### Output to the plot for mom revenue growth############################ 
  
  output$mom_revenue_growth <- renderPlotly({
    if(input$inputYear1=="All"){
      gg <- ggplot(data = g, aes(x=date, y=growth, text = paste(growth, "%"))) + 
        geom_bar(stat="identity", aes(fill = growth > 0 ))  + 
        scale_fill_manual(values = c('red', 'darkgreen') ) +
        scale_x_date(labels = date_format("%Y"), date_breaks = "year") + 
        scale_y_continuous(labels = scales::unit_format("%")) +
        labs(x="Year", y="Revenue Growth") +
        theme(legend.position="none")
      
      ggplotly(gg, tooltip = c("text"))
    }
    else if(input$inputYear1=="2019") {
      
      gg <- ggplot(data = subset(g, year(date) == 2019), aes(x=date, y=growth, text = paste(growth, "%"))) + 
        geom_bar(stat="identity",  aes(fill = growth > 0 ), width=2)  + 
        scale_fill_manual(values = c('red', 'darkgreen') ) +
        scale_x_date(labels = date_format("%b"), date_breaks = "1 month") + 
        scale_y_continuous(labels = scales::unit_format("%")) +
        labs(x="Month", y="Revenue Growth") +
        theme(legend.position="none") 
      # geom_text(data = subset(g, year(date) == 2019), aes(x = date, y = growth, label = paste(growth,"%"))
      #           ,position = position_dodge(0.9), vjust = -1, size = 3)
      
      ggplotly(gg, tooltip = "text")
    } 
    else{
      # min <- paste(input$inputYear1,"1-1",sep="-")
      # max <- paste(input$inputYear1,"12-1",sep="-")
      
      gg <- ggplot(data = subset(g, year(date) == input$inputYear1), aes(x=date, y=growth, text = paste(growth, "%"))) + 
        geom_bar(stat="identity",  aes(fill = growth > 0 ))  + 
        scale_fill_manual(values = c('red', 'darkgreen') ) +
        scale_x_date(labels = date_format("%b"), date_breaks = "1 month", expand = c(0,0)) + 
        scale_y_continuous(labels = scales::unit_format("%")) +
        labs(x="Month", y="Revenue Growth") +
        theme(legend.position="none") 
      # geom_text(data = subset(g, year(date) == input$inputYear1)
      #           , aes(x = date, y = growth, label = paste(growth,"%"))
      #           ,position = position_dodge(0.9), vjust = -1, size = 3)
      
      ggplotly(gg, tooltip = "text")
    }
  })
  
  
  ######################### For the data table############################    
  
  #create a data frame for the revenue table
  bmt <- data.frame(bm$year, bm$month, bm$revenue)
  colnames(bmt) <- c("Year", "Month", "Revenue ($)")
  
  #output to the table for monthly revenue
  
  output$tbl4 <-DT::renderDataTable({
    if(input$inputYear1=="All") {
      DT::datatable(bmt, options = list(pageLength = 12
                                        , lengthMenu = list(c(12, 24, 36, -1), c('12', '24', '36', 'All'))))} 
    else {
      DT::datatable(data = subset(bmt, Year == input$inputYear1), 
                    options = list(rownames= FALSE, searching = FALSE, pageLength = 12,lengthMenu = list(c('12'))))
    }
  })
  
  
  #output$tbl3 <- renderDataTable({on.exit(dbDisconnect(conn), add = TRUE)
  # main})
  
  
  
  
 # lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
  
  
  
    
##############################################################################################################
##############################################################################################################
################################## DASHBOARD PAGE 4-ACTIVATION RATE###########################################
    
    
    #####################FOR THE DATA TABLE#########################################################
    activation_table<-function(){
      conn2 <- dbConnect(
        drv = RMySQL::MySQL(),
        dbname = "shinyappsdev",
        host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
        username = "keyo",
        password = "j9bA&bAWbGl2")
      
      myQuery<-paste0("select MONTH(STR_TO_DATE(transaction_date,'%m/%d/%Y')) as 'Month',(count(DISTINCT user_id)/count(DISTINCT `User ID`))*100 as 'Activation_Rate' 
        from transactions t,users u
      where YEAR(STR_TO_DATE(transaction_date,'%m/%d/%Y'))=",input$inputYear2,"
                      and
                      u.created_at<=transaction_date 
                      group by MONTH(STR_TO_DATE(transaction_date,'%m/%d/%Y'))")
      actTable<-(fetch(dbSendQuery(conn2,myQuery)))
      dbDisconnect(conn2)
      actTable
    }
    actTable<-reactive({activation_table()})
    
    output$act_tbl <- renderDataTable(actTable())
    
    #output to the plot
     output$Activation <- renderPlotly({
       gg1<-ggplot(data=actTable(), aes(x=Month, y=Activation_Rate, group=1)) +theme_classic()+
         geom_point(stat='summary', fun.y=sum,size=2,color="blue") +
         stat_summary(fun.y=sum, geom="area",size=1.25,fill="Sky Blue")+
         scale_colour_manual(values="#E1B378")+ scale_x_discrete(limits = month.abb)
    
       ggplotly(gg1,tooltip = c("Activation_Rate"))

    })
    
  
     ###################################################################################
     ######################### DASHBOARD PAGE 5-SUCCESS RATE############################
     
     
     
     #####################FOR THE DATA TABLE#########################################################
     success_table<-function(){
       conn2 <- dbConnect(
         drv = RMySQL::MySQL(),
         dbname = "shinyappsdev",
         host = "shinyappsdev.c2skb1ax7plf.us-east-1.rds.amazonaws.com",
         username = "keyo",
         password = "j9bA&bAWbGl2")
       
       myQuery<-paste0("select terminal_id,succeeded,count(t.id) as 'Successful' from transactions t
                        where MONTH(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputMonth3," 
                        and YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear3," and succeeded=1
                        group by terminal_id,succeeded")
       successTable<-(fetch(dbSendQuery(conn2,myQuery)))
       for(i in 1:12)
       {
         if(successTable[[terminal_id]]!=i)
         {
           successTable<-c(i,0,0)
         }
       }
       myQuery<-paste0("select terminal_id,succeeded,count(*) as 'Total' from transactions t
                        where MONTH(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputMonth3," 
                       and YEAR(STR_TO_DATE(`transaction_date`,'%m/%d/%Y'))=",input$inputYear3,"
                       group by terminal_id,succeeded")
       successTable1<-(fetch(dbSendQuery(conn2,myQuery)))
       dbDisconnect(conn2)
       successTable<-data.frame(successTable,successTable1)
       successTable$Success_Rate<-(successTable$Successful/successTable$Total)*100
       successTable$terminal_id.1<-NULL
       successTable
     }
     successTable<-reactive({success_table()})
     
     
     
     output$suc_tbl <- renderDataTable(successTable())
     
     # output$suck <- renderPlot({
     #   ggplot(data = successTable(),
     #          aes(x=factor(terminal_id), y=Success_Rate)) +
     #     geom_bar(position = "dodge", stat="identity") + ylab("Success Percentage") +
     #     xlab("Terminals") + theme(legend.position="bottom"
     #                            ,plot.title = element_text(size=15, face="bold")) +
     #     ggtitle("Successful Transaction Rate by terminal")
     # })
     
     output$suck <- renderPlotly({
       gg1<-ggplot(data=successTable(), aes(x=factor(terminal_id), y=Success_Rate, group=1)) +theme_classic()+
         geom_point(stat='summary', fun.y=sum,size=2,color="blue") +
         stat_summary(fun.y=sum, geom="area",size=1.25,fill="maroon")+
         scale_colour_manual(values="#E1B378")+ylab("Success Percentage") +
              xlab("Terminals") + theme(legend.position="bottom"
                                     ,plot.title = element_text(size=15, face="bold")) +
              ggtitle("Successful Transaction Rate by terminal")
       
       ggplotly(gg1,tooltip = c("Success_Rate"))
       
     })



lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)


    
}