ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Sales Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        selectInput("Store", "Store:", choices = unique(walmart_full$Store),
                                    selected = 43),
                        
                        
                        selectInput("Dept", "Department:", choices = unique(walmart_full$Dept), 
                                    selected = 1),
                        
                        
                        dateRangeInput("Date", strong("Date range"), start = "2010-02-05", end = "2012-10-26",
                                       min = "2010-02-05", max = "2012-10-26", format = "yyyy/mm", startview = "year")
                        
                        
                      )
                    ),
                    dashboardBody(
                      tabsetPanel(
                        tabPanel(
                          "Plots", 
                          fluidRow(
                            valueBoxOutput("TotalRevenue"),
                            valueBoxOutput("StoreRevenue"),
                            valueBoxOutput("DeptRevenue")
                          ),
                          fluidRow(
                            box(plotOutput("SalePlotOverall"), width = 12)
                          ),
                          fluidRow(
                            box(plotOutput("TopStores")),
                            box(plotOutput("TopDept"))
                          )
                        )
                      )
                    )
)
