# Installing and loading packages

if(!require("install.load")){
  install.packages("install.load")
}
library('install.load')
install_load("readr", "readxl", "tidyr", "dplyr", "stringr", "tidyverse", "knitr", 
             "rmarkdown", "ggplot2","lubridate","kableExtra","shiny","shinythemes",
             "plotly","DT","shinyWidgets","leaflet", "leafpop", "leaflet.extras","shinyjs")

# Importing the dataset
Final_dataset_Group_20 <-read_csv("Final_dataset_Group_20.csv")

#Task a)
Data1 <-Final_dataset_Group_20 %>% drop_na(Community, Longitude, Latitude) %>% 
  group_by(ID_Car,Community)

Data2 <- Data1  %>% filter(Defective_Part == 1) %>%
  filter(!duplicated(ID_Car)) %>% ungroup(ID_Car) %>% count(Community, sort = T)

Data3 <- Data1 %>% filter(!duplicated(ID_Car)) %>% ungroup(ID_Car) %>%
  count(Community, sort = T)

Data4 <- Data2 %>% left_join(Data3, "Community")
Data5 <- Data1 %>% ungroup(ID_Car) %>% dplyr::select(Community, Postal_Code, Longitude,Latitude)
Data6 <- Data4 %>% left_join(Data5, "Community")  %>%
  unique() %>% 
  rename("Number of Defective Cars" = `n.x`,
         "Number of Registered Cars" = `n.y`) %>%
  ungroup() %>%
  mutate("Share of Defective Cars" = paste((round((`Number of Defective Cars`/`Number of Registered Cars`),4)*100),"%", sep = "")) %>%
  rename(Postcode = Postal_Code) %>%
  relocate(Community, Postcode,Longitude,Latitude)

# Task b)

Final_task_b <- Final_dataset_Group_20 %>% 
  mutate(Production_Date_Part = as.Date(Production_Date_Part)) %>%
  mutate(year = year(ymd(Production_Date_Part))) %>%
  mutate(month = paste0(month(ymd(Production_Date_Part), label=TRUE, abbr = FALSE), "-",year)) %>% 
  mutate(week = paste0("Week Number ",week(ymd(Production_Date_Part)), "-",year)) %>%
  arrange(year,as.numeric(week)) 


# Task c)
Final_Data_C <- Final_dataset_Group_20 %>% filter(Defective_Part == 1) %>%
  dplyr::select(ID_Part, Mileage_Part, Community) %>% arrange(Community)







# Web application
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyjs::useShinyjs(),
                tags$head(tags$style('{font-family: Source Sans Pro;}')),
                titlePanel(div(h1(strong("Case Study Group 20"), 
                                  img(height = 75, width = 150, src='Logo Company.png',
                                      style="position:absolute;right:15px"))
                )),
                br(),
                br(),
                navbarPage("217 Automotive Metal",
                           tabPanel(icon("house"),
                                    h3(strong("Welcome to the web application of our vehicle body parts company: 
                                       217 Automotive Metal")), 
                                    "Recently, there have been more and more customer 
                                    complaints about premature rust spots on some of our products. The cause of the 
                                    premature rust spots lies in the purchase of steel whose composition did not comply 
                                    with the our company’s specifications. An internal investigation on our part revealed 
                                    that this steel was processed in our plants in the period from June 1, 2012 to July
                                    31, 2014. The purpose of this web application is that customers and vehicle owners
                                           are able to identify certain information related to this incident.",
                                    div(style = "height:40px;;", HTML("<br>")),
                                    HTML('<center><img src="HomePic.jpg" width="50%"></center>'),
                           ),
                           tabPanel("Damage hotspots",
                                    h3(strong("Damage Hotspot heatmap section:")),
                                    "In this section, customers can visualize damage hotspots across the country. 
                                    Furthermore, by clicking on a desired community, popups will display. These popups 
                                    will contain the following information: Community, Postcode, Longitude, 
                                    Latitude, Number of Defective Cars, Number of Registered Cars, 
                                    Share of Defective Cars.",
                                    div(style = "height:50px;;", HTML("<br>")),
                                    bootstrapPage(
                                      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                                      leafletOutput("MapHS"),
                                    )),
                           tabPanel("Delivered Quantities",
                                    h3(strong("Overview of production quantities delivered to vehicles manufacturers:")),
                                    "In this section, it is possible to calculate the number of parts delivered in a selectable timeframe of years, 
                                    months or weeks. The barplot will show the quantities delivered to the different car manufacturer plants in our 
                                    dataset.",
                                    div(style = "height:50px;;", HTML("<br>")),
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "year", label = "Select a year:", choices = unique(Final_task_b$year)),
                                        selectInput(inputId = "month", label = "Select a month:", choices = unique(Final_task_b$month)),
                                        selectInput(inputId = "week", label = "Select a week of the year:", choices = unique(Final_task_b$week)),
                                        
                                        
                                        actionButton("SBb", "Submit", class = "btn btn-primary")
                                      ),
                                      mainPanel(
                                        shinyjs::hidden(
                                          div(
                                            id = "HideB",
                                            fluidRow(
                                              column(br(), plotlyOutput("stackplot"),br(),width=12,
                                                     style="border:0px solid black")))
                                        )))),
                           tabPanel("Useful life body parts",
                                    h3(strong("Mileage before rupture:")),
                                    "This section allows to check the number of kilometers that vehicles, 
                                    registered in a specific community, travelled before they were found defective. 
                                    Two communities can be selected at a time.",
                                    div(style = "height:50px;;", HTML("<br>")),
                                    sidebarLayout(
                                      sidebarPanel(h3("Select your communities:"),
                                                   selectizeInput(inputId = "community1", label = "1st Community:", 
                                                                  choices = unique(Final_Data_C$Community),
                                                                  options = list(maxOptions = 4905),
                                                                  selected = "KOELN"),
                                                   selectizeInput(inputId = "community2", label = "2nd Community:", 
                                                                  choices = unique(Final_Data_C$Community),
                                                                  options = list(maxOptions = 4905),
                                                                  selected = "DORTMUND"),
                                                   actionButton("SBc", "Submit", class = "btn btn-primary")
                                      ),
                                      mainPanel(
                                        shinyjs::hidden(
                                          div(
                                            id = "HideC",
                                            fluidRow(
                                              column(br(), plotlyOutput("boxplotC"),br(),width=12,
                                                     style="border:0px solid black"))
                                          )))
                                    )),
                           tabPanel("vehicle ID",
                                    h3(strong("Car Checking section:")),
                                    "In this section, it should be possible for you to check with your car ID, 
                                       whether your vehicle was affected by this incident or not. If your car contained
                                    a part that was produced during the period in which the non-compliant steel was used 
                                    a list of components from our company installed in your car will be displayed. You will
                                    be shown whether this parts where listed as defective or not. Furthermore a message 
                                    will be printed for you, so you can check whether your car was affected by this incident
                                    or not.",
                                    div(style = "height:50px;;", HTML("<br>")),
                                    sidebarLayout(
                                      sidebarPanel(width = 3,
                                                   textInput(inputId = "ID_Car",
                                                             label = "Please insert your vehicle/car ID",
                                                             value = "",
                                                             placeholder = "Car ID",
                                                             width = NULL),
                                                   actionButton("SBd", "Submit", class = "btn btn-primary")
                                      ),
                                      fluidRow(
                                        mainPanel(
                                          shinyjs::hidden(
                                            div(
                                              id = "HideD",
                                              fluidRow(
                                                column(br(), h2(strong(textOutput("ResultD"))),br(),width=12)
                                              ),
                                              fluidRow(DTOutput("ObservationD")))
                                          )))
                                    )
                           ),
                           tabPanel("Dataset",
                                    basicPage(
                                      h3(strong("Final dataset Group 20")),
                                      DTOutput("Final_Data")
                                    ))
                )
)

server <-function(input, output) {
  
  #Task a)
  output$MapHS <-renderLeaflet({
    leaflet(Data6) %>% addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addHeatmap(lng= ~Longitude,lat= ~Latitude, intensity = ~Data6$`Number of Defective Cars`, blur = 5, max = 2000, 
                 radius = 20) %>%
      addCircleMarkers(lng = ~Longitude, lat = ~Latitude, 
                       fillOpacity = 0, weight = 0, popup = popupTable(Data6))
    
  })
  
  #Task b)
  
  output$stackplot <- renderPlotly({
    input$SBb
    input_B <-  c(isolate(input$year), isolate(input$month), isolate(input$week))
    OEM1_FacNum11_year <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 11) %>% filter(year == isolate(input$year))
    
    OEM1_FacNum12_year <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 12) %>% filter(year == isolate(input$year))
    
    OEM2_FacNum21_year <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 21) %>% filter(year == isolate(input$year))
    
    OEM2_FacNum22_year <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 22) %>% filter(year == isolate(input$year))
    
    
    
    OEM1_FacNum11_month <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 11) %>% filter(month == isolate(input$month)) 
    
    OEM1_FacNum12_month <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 12) %>% filter(month == isolate(input$month))
    
    OEM2_FacNum21_month <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 21) %>% filter(month == isolate(input$month))
    
    OEM2_FacNum22_month <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 22) %>% filter(month == isolate(input$month))
    
    
    
    OEM1_FacNum11_week <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 11) %>% filter(week == isolate(input$week))
    
    OEM1_FacNum12_week <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 12) %>% filter(week == isolate(input$week))
    
    OEM2_FacNum21_week <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 21) %>% filter(week == isolate(input$week))
    
    OEM2_FacNum22_week <- Final_task_b %>% dplyr::select(Factory_Number, week, month, year) %>% 
      filter(Factory_Number == 22) %>% filter(week == isolate(input$week))
    
    y_vector11 <- c(nrow(OEM1_FacNum11_year), nrow(OEM1_FacNum11_month), nrow(OEM1_FacNum11_week))
    y_vector12 <- c(nrow(OEM1_FacNum12_year), nrow(OEM1_FacNum12_month), nrow(OEM1_FacNum12_week))
    y_vector21 <- c(nrow(OEM2_FacNum21_year), nrow(OEM2_FacNum21_month), nrow(OEM2_FacNum21_week))
    y_vector22 <- c(nrow(OEM2_FacNum22_year), nrow(OEM2_FacNum22_month), nrow(OEM2_FacNum22_week))
    
    plot_ly(x = ~input_B, y = ~y_vector11, type = 'bar', name = '11-OEM:1') %>% 
      add_trace(y = ~y_vector12, name = '12-OEM:1') %>%
      add_trace(y = ~y_vector21, name = '21-OEM:2') %>%
      add_trace(y = ~y_vector22, name = '22-OEM:2') %>%
      layout(yaxis = list(title = 'Quantity'), xaxis =list(title = "Production period"), barmode = 'group') %>%
      layout(legend=list(title=list(text='<b> Plant Number </b>')))
  }) 
  
  observeEvent(input$SBb,{
    shinyjs::show("HideB")
  })
  
  #Task c) 
  
  output$boxplotC <- renderPlotly({
    input$SBc
    dsC <- Final_Data_C %>%
      filter(Community %in% c(isolate(input$community2),isolate(input$community1))) %>%
      mutate(Order = case_when(Community == isolate(input$community1) ~ paste0(1,"-", isolate(input$community1))
                               ,Community == isolate(input$community2) ~ paste0(2,"-", isolate(input$community2))))%>%
      arrange(Order) %>%
      mutate(Mileage_Part = round((Mileage_Part/1000),2))
    plot_ly(dsC, y = ~Mileage_Part, color = ~Order, type = "box") %>%
      layout(yaxis = list(title = 'Mileage (in 1000Km)'), xaxis =list(title = "Communities"))%>%
      layout(legend=list(title=list(text='<b> Communities </b>')))
  })
  
  observeEvent(input$SBc,{
    shinyjs::show("HideC")
  })
  
  
  #Task d)
  
  output$ResultD <-renderText({
    input$SBd
    dsD <- Final_Data_D <- Final_dataset_Group_20 %>% filter(ID_Car == isolate(input$ID_Car))
    vecD <-case_when(
      dsD$Defective_Part == 1 ~ TRUE
      ,TRUE ~ FALSE
    )
    if (TRUE %in% vecD){
      print("We must sadly inform you that you car contains a defective part!")
    } else if (length(vecD) == 0){
      print("Your car did not contain any part of our company which was produced
              in the affected period.")
    }
    else{print("Fortunately, you car was not affected by this situation!")}
  }) 
  
  output$ObservationD <- renderDT({
    input$SBd
    dsD <- Final_Data_D <- Final_dataset_Group_20 %>% filter(ID_Car == isolate(input$ID_Car))
    dsD<-dsD %>%
      dplyr::select(ID_Part, Part_Type, Defective_Part, ID_Car)
    DT::datatable(dsD,
                  filter = 'top', extensions = 'Buttons',
                  options = list(pageLength = nrow(dsD),
                                 buttons = list(list(extend = 'colvis', targets = 0, visible = FALSE)),
                                 dom = 'lBfrtip',
                                 fixedColumns = TRUE),
                  rownames = FALSE)
  })  
  
  observeEvent(input$SBd,{
    shinyjs::show("HideD")
  })
  
  #Task e)
  
  output$Final_Data <- renderDT({
    DT::datatable(
      Final_dataset_Group_20,
      filter = 'top', extensions = c('Buttons', 'Scroller'),
      options = list(scrollY = 650,
                     scrollX = 500,
                     deferRender = TRUE,
                     scroller = TRUE,
                     paging = TRUE,
                     pageLength = 25,
                     buttons = list(
                       list(extend = 'colvis', targets = 0, visible = FALSE)
                     ),
                     dom = 'lBfrtip',
                     fixedColumns = TRUE), 
      rownames = FALSE)
  })
  
}



shinyApp(ui = ui, server = server)




