server <- function(input, output) {
  
  #Value box for total revenue
  output$TotalRevenue <- renderValueBox({
    valueBox(
      paste("$",round((sum(walmart_full %>% 
                             filter(Monthly_Date >= input$Date[1] & Monthly_Date <= input$Date[2]) %>%
                             select(Monthly_Sales))/1000000),2),"M"), 
      subtitle = "Total Revenue (Million)",
      #      icon = icon("credit-card"),
      color ="olive"
    )
  })
  
  #Value box for Store Revenue
  output$StoreRevenue <- renderValueBox({
    valueBox(
      paste("$",round((sum(walmart_full %>% 
                             filter(Store == input$Store & Monthly_Date >= input$Date[1] & Monthly_Date <= input$Date[2]) %>%
                             select(Monthly_Sales))/1000000),2),"M"), 
      subtitle = paste("Revenue for Store#",input$Store, "(Million)"),
      #     icon = icon("map-marker"),
      color ="green"
    )
  })
  
  #Value box for Dept. Revenue
  output$DeptRevenue <- renderValueBox({
    valueBox(
      paste("$",round((sum(walmart_full %>% 
                             filter(Store == input$Store & Dept == input$Dept & Monthly_Date >= input$Date[1] 
                                    & Monthly_Date <= input$Date[2]) %>%
                             select(Monthly_Sales))/1000000),2),"M"), 
      subtitle = paste("Revenue for Dept.#", input$Dept, " of Store#", input$Store, "(Million)"),
      #      icon = icon("barcode"),
      color ="teal"
    )
  })
  
  #Overall sales plot for selected Store        
  output$SalePlotOverall <- renderPlot({
    x <- walmart %>% filter(Store == input$Store) %>%
      group_by(Monthly_Date) %>% 
      summarise(Sales = sum(Weekly_Sales)) %>%
      arrange(Monthly_Date)
    
    a <- x %>% filter(year(x$Monthly_Date)==2010)
    b <- x %>% filter(year(x$Monthly_Date)==2011)
    c <- x %>% filter(year(x$Monthly_Date)==2012)
    
    plot(a$Monthly_Date, a$Sales/1000000, 
         type = "l", 
         lwd= 3,
         main = paste("Overall Sales for Store #", input$Store),
         xlab = "", 
         ylab = "Total Revenue (Million)",
         col= "maroon"
    )
    par(new=TRUE)
    plot(b$Sales/1000000, 
         type = "l", 
         lwd = 3, 
         xlab = "", 
         ylab = "",
         axes = FALSE,
         col = "aquamarine4"
    )
    par(new=TRUE)
    plot(c$Sales/1000000, 
         type = "l", 
         lwd = 3,
         xlab = "", 
         ylab = "",
         axes = FALSE,
         col="chocolate2"
    )
    legend("topleft", 
           c("2010","2011","2012"),
           fill = c("maroon","aquamarine4","chocolate2"))
  })    
  
  output$TopStores <- renderPlot({
    x <- walmart_full %>% filter(Monthly_Date >=input$Date[1] & Monthly_Date <=input$Date[2]) %>%    
      group_by(Store) %>% 
      summarise(total_sales = sum(Monthly_Sales)) %>%
      arrange(desc(total_sales))
    
    a<- barplot(x$total_sales[1:5]/1000000,
                main= paste(input$Date[1],"-",input$Date[2],":","Sales of Top 5 Stores"),
                axisnames = TRUE,legend.text = TRUE,
                space = 0.5,
                xlab = "Store number",
                ylab="Total Store Sales (Million)",
                ylim = c(0,350),
                names.arg = x$Store[1:5],
                axes = TRUE,
                col ="cadetblue4"
    )
    text(a, x$total_sales[1:5]/1000000, labels = paste(round(x$total_sales[1:5]/1000000,2),"$"), pos=3)
    a
    
  })
  
  output$TopDept <- renderPlot({
    x <- walmart_full %>% filter(Monthly_Date >=input$Date[1] & Monthly_Date <=input$Date[2]) %>%    
      group_by(Dept) %>% 
      summarise(total_sales = sum(Monthly_Sales)) %>%
      arrange(desc(total_sales))
    
    b <- barplot(x$total_sales[1:5]/1000000,
                 main= paste(input$Date[1],"-",input$Date[2],":","Sales of Top 5 Departments"),
                 axisnames = TRUE,legend.text = TRUE,
                 space = 0.5,
                 xlab = "Department number",
                 ylab="Total Dept Sales (Million)",
                 ylim = c(0,500),
                 names.arg = x$Dept[1:5],
                 col ="aquamarine3")
    
    text(b, x$total_sales[1:5]/1000000, labels=paste(round(x$total_sales[1:5]/1000000,2),"$"), pos = 3)
    
  })
  
  
}