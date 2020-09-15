
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(readr)
library(reshape2)
library(scales)
library(lubridate)
library(sf)



#loading ZWE data
zwe_data<- read_csv("./clean_data.csv")
cnt<-zwe_data %>% melt(id.vars="district")

#loading ZWE shapefile
district_shp<-readOGR("./shp/zwe_admbnda_adm3_zimstat_ocha_20180911.shp")


zwe_data$start <- as.Date(zwe_data$start,
                          "%Y/%m/%d")

# create variables of the week and month of each observation:
zwe_data$month <- as.Date(cut(zwe_data$start,
                              breaks = "month"))

zwe_data$start_dow <- as.Date(cut(zwe_data$start,
                                  breaks = "week",
                                  start.on.monday = FALSE)) # changes weekly break point to Sunday

summary_table <- zwe_data %>%
    group_by(admin3_p_code) %>% 
    summarise(Popultn = sum(no_of_individual_targeted),
              Ppltn_D = sum(total_no_of_individuals_reached),
              Ml_Pplt = sum(no_of_boys_reached),
              Fml_Ppl = sum(no_of_girls_reached),
              Intrsx_ = sum(no_of_women_reached),
              Nmbr__H = sum(no_of_women_reached),
              Avrg_H_ = mean(intial_4w_recotd)
    )
district_shp@data <- district_shp@data %>% 
    inner_join(summary_table, by = c("ADM3_PCODE" = "admin3_p_code"))


# Define UI 
ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(
        title = "ZIMBABWE: Aftermath of Cyclone Idai (2019)", titleWidth = 500,
        tags$li(actionLink("ONA", 
                           label = "", 
                           icon = icon("users"),
                           onclick = "window.open('https://ona.io/jobs/data-analytics-lead.html')"),
                class = "dropdown"),
        tags$li(actionLink("GitHub", 
                           label = "", 
                           icon = icon("github"),
                           onclick = "window.open('https://github.com/vmandela99')"),
                class = "dropdown")
    ),
    
    dashboardSidebar(
        
        sidebarUserPanel("ONA",
                         subtitle = "Data Solutions"
        ),
        sidebarMenu(
            menuItem(
                "Introduction",
                tabName = "intro",
                icon = icon("info")
            ),
            
            menuItem(
                "Map",
                tabName = "map",
                icon = icon("map")
            ),
            
            menuItem(
                "Data",
                tabName = "data",
                icon = icon("table")
            ),
            
            menuItem(
                "About ONA",
                tabName = "Company",
                icon = icon("building")
            )
            
        )             
        
    ),
    
    dashboardBody(
        tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }
                              '))),
        tabItems(
            
            tabItem(
                tabName = "intro",
                
                
                fluidRow(
                    
                    h2(HTML("<strong>Overview.</strong>")),
                    h3(tags$p("The world’s leading humanitarian, development and global health organizations rely on Ona to improve their impact and accountability. 
                     We build technology that affords new opportunities for governments and development organizations and actors to be increasingly collaborative, data-driven, and accountable to the people they serve. 
                     We also make tools that provide opportunities for organizations to make better use of their data to help address some of the world’s greatest challenges, including: maternal and child health, education, community resilience, agriculture, environmental sustainability, access to infrastructure and government accountability.")),
                    h3(tags$p("The plot below shows the population target from March to May 2019.")),
                    plotlyOutput("lineplot",width = 1050),
                    h2(HTML("<strong>Purpose.</strong>")),
                    h3(tags$p("The main purpose of this is coordinating among different actors as a fundamental need in responding to large-scale natural
disasters.")),
                    h2(HTML("<strong>Contents.</strong>")),
                    h3(tags$p("Facts and figures through Interactive visualization dashboards and data tables"))
                )
                #end fluidrow
                
            ), #end tabname
            
            tabItem(
                tabName = "map",
                
                fluidRow(
                    
                    valueBoxOutput(
                        "organisation"
                        
                    ),#end box
                    
                    valueBoxOutput(
                        "partner"
                        
                    ), #end box
                    
                    valueBoxOutput(
                        "district"
                        
                    ),
                    
                    valueBoxOutput(
                        "activities"
                        
                    ),
                    valueBoxOutput(
                        "target"
                        
                    ),
                    
                    valueBoxOutput(
                        "reach"
                        
                    )
                    
                ),
                
                fluidRow(
                    column(width = 3,
                           selectInput(
                               inputId = "stats",
                               label = "Select Indicator",
                               choices =c(
                                   "Target number"=1,
                                   "Total response number"=2,
                                   "Number of boys"=3,
                                   "Number of girls"=4,
                                   "Number of women"=5,
                                   "Number of men"=6,
                                   "Average Initial 4w recorded"=7
                               ),selected = 1
                               
                           ) 
                           
                           
                    ) #end column
                    
                    
                ),
                #end row
                fluidRow(
                    column(
                        width = 6,
                        #box(
                        # title = "MAP",
                        # status = "primary",solidHeader = TRUE,
                        # width = NULL,height = 600,collapsible = TRUE,
                        leafletOutput("maps",height = 500)
                        #)
                    ),#end column
                    
                    column(width = 3,
                           plotlyOutput("top",height = 500)
                    ),
                    
                    column(width = 3,
                           plotlyOutput("bottom",height = 500)
                    )
                    
                    
                    
                )#end row
                
                
                
            ), #end tab item
            
            tabItem(
                tabName = "data",
                
                div(
                    h1(strong("Dataset.")),
                    h3("The table below displays the number of target population, the total responses,boys, girls, women, men and  average intial recorded population in Zimbabwe. 
             ")
                ),
                dataTableOutput("table")
            ),
            tabItem(
                tabName = "author",
                fluidRow(
                    br(),
                    
                    img(src ="logo.png", width = "17%", style = "display: block; margin-left: auto; margin-right: auto;")
                    
                ),
                
                fluidRow(
                    h3(strong("ONA"), style = "text-align: center"),
                    h4("https://ona.io/contact.html", style = "text-align: center")
                ),
                
                hr(),
                fluidRow(column(5, ""),
                         column(
                             3,
                             tags$h3(
                                 HTML('&nbsp;'),
                                 HTML('&nbsp;'),
                                 HTML('&nbsp;'),
                                 tags$a(
                                     href = 'https://www.linkedin.com/company/ona-io/',
                                     img(
                                         src = 'linkedin.png',
                                         height = "50px"
                                     )
                                 ),
                                 HTML('&nbsp;'),
                                 tags$a(href = 'https://ona.io/contact.html', img(
                                     src = 'logo.png',
                                     height = "50px"
                                 ))
                             )
                         )),
                
                
                fluidRow(
                    column(2, ""),
                    column(
                        1,
                        h3(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
                        br(),
                        br(),
                        h3(icon("globe"), style = "text-align: right; line-height: 200%"),
                        br(),
                        h3(icon("heart"), style = "text-align: right; line-height: 170%;")
                    ),
                    column(
                        6,
                        h4(
                            "Our mission is to ensure equitable access to services for those who need them",
                            style = "text-align: left; line-height: 150%;"
                        ),
                        br(),
                        h4(
                            " We believe technology is a springboard to transform international development, allowing organizations to use data to overcome the greatest challenges and identify the best opportunities.",
                            style = "text-align: left; line-height: 150%;"
                        ),
                        br(),
                        h4(
                            "Software as a service, International development, Humanitarian technology, and Software engineering. ",
                            style = "text-align: left; line-height: 150%;"
                        )
                        
                    ),
                    
                    column(3, "")
                    
                    
                    
                )#end row
                
            )
        )#end tab items
        
    )
    
    
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    
    output$lineplot<-renderPlotly({
        ggplotly(
            ggplot(data = zwe_data
                   ,aes(x = month,y = total_no_of_individuals_reached, color = district))+
                stat_summary(fun = sum, # adds up all observations for the month
                             geom = "line") + # or "line"
                scale_x_date(
                    labels = date_format("%Y-%m"),
                    breaks = "1 month") + # custom x-axis labels
                labs(
                    title = "Individuals reached from March 15th to May 24th 2019.",
                    x= "Months",
                    y= "Individuals reached.",
                    color = "Districts"
                ) +
                theme_minimal()  +
                expand_limits(y = -10000)
            
        )
    })
    
    output$organisation<-renderValueBox({
        valueBox(
            n_distinct(zwe_data$lead_organization), "organisations working with", icon = icon("building"),
            color = "purple"
        )
    })
    
    output$partner<-renderValueBox({
        valueBox(
            n_distinct(zwe_data$implementing_partner), "implementing partners in", icon = icon("handshake"),
            color = "blue"
        )
    })
    
    output$district<-renderValueBox({
        valueBox(
            n_distinct(zwe_data$district), "districts, engaged in over", icon = icon("chart-network"),
            color = "yellow"
        )
    })
    
    output$activities<-renderValueBox({
        valueBox(
            n_distinct(zwe_data$response_activity), "Response acivities", icon = icon("briefcase"),
            color = "green"
        )
    })
    
    output$target<-renderValueBox({
        valueBox(
            comma(sum(zwe_data$no_of_individual_targeted)), "were eople targeted while", icon = icon("users"),
            color = "orange"
        )
    })
    
    output$reach<-renderValueBox({
        valueBox(
            comma(sum(zwe_data$total_no_of_individuals_reached)), " were reached", icon = icon("users"),
            color = "orange"
        )
    })
    
    
    #rendering the basemap
    output$maps<-renderLeaflet(
        leaflet(district_shp) %>%
            setView(lng=31.053028,lat=-17.824858,zoom = 6) %>%
            addPolygons(
                color = ~palpop(Popultn),
                smoothFactor = 0.5,
                weight = 2, opacity = 1.0,
                fillOpacity = 1.0,
                highlightOptions = highlightOptions(
                    weight = 1,
                    color = "brown",
                    fillOpacity = 0.7,
                    bringToFront = TRUE
                ),
                label = paste(
                    "<strong>District:</strong>",district_shp$ADM2_EN,
                    "<br>",
                    "<strong>Total Population:</strong>",district_shp$Popultn
                    
                ) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                          padding = "3px 8px"), 
                                             textsize = "13px", direction = "auto"),
                
                popup = ~paste(
                    "<strong>District:</strong>",ADM2_EN,
                    "<br>",
                    "<strong>Total target population:</strong>",Popultn
                    
                )
                
            ) %>%
            addLegend(title = "Total target population",
                      pal = palpop, values = district_shp$Popultn, opacity = 1)
        
    )
    
    
    
    #color functions
    #target population
    palpop<-colorBin("YlOrBr", district_shp$Popultn)
    
    #response population
    pal2<-colorBin("YlOrBr", district_shp$Ppltn_D)
    
    #number of boys reached
    pal3<-colorBin("YlOrBr", district_shp$Ml_Pplt)
    
    #number of girls reached
    pal4<-colorBin("YlOrBr", district_shp$Fml_Ppl)
    
    #number of women reached
    pal5<-colorBin("YlOrBr", district_shp$Intrsx_)
    
    #Number of men reached
    pal6<-colorBin("YlOrBr", district_shp$Nmbr__H)
    
    #Initial_4w_recotd
    pal7<-colorBin("YlOrBr", district_shp$Avrg_H_)
    
    
    observe({
        proxy<-leafletProxy("maps") %>% clearControls()
        if ("1" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color = ~palpop(Popultn),
                    smoothFactor = 0.5,
                    weight = 2, opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Total target population:</strong>",district_shp$Popultn
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Total target population:</strong>",Popultn
                        
                    )
                    
                ) %>%
                addLegend(title = "Total target population",
                          pal = palpop, values = district_shp$Popultn, opacity = 1)
        }
        
        else if ("2" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal2(Ppltn_D),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Number of people reached:</strong>",district_shp$Ppltn_D
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Number of people reached:</strong>",Ppltn_D
                        
                    )
                    
                ) %>%
                addLegend(title = "Population Density(per KM2)",
                          pal = pal2, values = district_shp$Ppltn_D, opacity = 1)
        }
        
        
        else if ("3" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal3(Ml_Pplt),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Number of boys:</strong>",district_shp$Ml_Pplt
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Number of boys:</strong>",Ml_Pplt
                        
                    )
                    
                ) %>%
                addLegend(title = "Number of boys",
                          pal = pal3, values = district_shp$Ml_Pplt, opacity = 1)
        }
        
        
        else if ("4" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal4(Fml_Ppl),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Number of girls:</strong>",district_shp$Fml_Ppl
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Number of girls:</strong>",Fml_Ppl
                        
                    )
                    
                ) %>%
                addLegend(title = "Number of girls",
                          pal = pal4, values = district_shp$Fml_Ppl, opacity = 1)
        }
        
        
        else  if ("5" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal5(Intrsx_),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Number of women reached:</strong>",district_shp$Intrsx_
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Number of women reached:</strong>",Intrsx_
                        
                    )
                    
                ) %>%
                addLegend(title = "Number of women reached",
                          pal = pal5, values = district_shp$Intrsx_, opacity = 1)
        }
        
        
        else if ("6" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal6(Nmbr__H),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Number of men reached:</strong>",district_shp$Nmbr__H
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Number of men reached:</strong>",Nmbr__H
                        
                    )
                    
                ) %>%
                addLegend(title = "Number of men reached",
                          pal = pal6, values = district_shp$Nmbr__H, opacity = 1)
        }
        
        
        
        else if ("7" %in% input$stats){
            proxy %>%
                addPolygons(
                    data = district_shp,
                    color =  ~pal7(Avrg_H_),
                    smoothFactor = 0.5,
                    weight = 2,
                    opacity = 1.0,
                    fillOpacity = 1.0,
                    highlightOptions = highlightOptions(
                        weight = 1,
                        color = "brown",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                    ),
                    label = paste(
                        "<strong>District:</strong>",district_shp$ADM2_EN,
                        "<br>",
                        "<strong>Initial recorded average number:</strong>",district_shp$Avrg_H_
                        
                    ) %>% lapply(htmltools::HTML),
                    labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                              padding = "3px 8px"), 
                                                 textsize = "13px", direction = "auto"),
                    popup = ~paste(
                        "<strong>District:</strong>",ADM2_EN,
                        "<br>",
                        "<strong>Initial recorded average number:</strong>",Avrg_H_
                        
                    )
                    
                ) %>%
                addLegend(title = "Initial recorded average number",
                          pal = pal7, values = district_shp$Avrg_H_, opacity = 1)
        }
        
    })
    
    
    
    
    output$top<-renderPlotly({
        if("1" %in% input$stats){
            zwe_data %>% select(district,no_of_individual_targeted) %>%
                arrange(desc(no_of_individual_targeted)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,no_of_individual_targeted),y=no_of_individual_targeted))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_individual_targeted))+
                labs(
                    title = "Top 5 counties",
                    y="Total target population",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("2" %in% input$stats){
            zwe_data %>% select(district,total_no_of_individuals_reached) %>%
                arrange(desc(total_no_of_individuals_reached)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,total_no_of_individuals_reached),y=total_no_of_individuals_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=total_no_of_individuals_reached))+
                labs(
                    title = "Top 5 counties",
                    y="Response number",
                    x="District"
                )+
                coord_flip()
        }
        
        else if("3" %in% input$stats){
            zwe_data %>% select(district,no_of_boys_reached) %>%
                arrange(desc(no_of_boys_reached)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,no_of_boys_reached),y=no_of_boys_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_boys_reached))+
                labs(
                    title = "Top 5 counties",
                    y="Number of boys",
                    x="District"
                )+
                coord_flip()
            
        }  
        
        else if("4" %in% input$stats){
            zwe_data %>% select(district,no_of_girls_reached) %>%
                arrange(desc(no_of_girls_reached)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,no_of_girls_reached),y=no_of_girls_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_girls_reached))+
                labs(
                    title = "Top 5 counties",
                    y="Number of girls",
                    x="District"
                )+
                coord_flip()
            
        }  
        
        else if("5" %in% input$stats){
            zwe_data %>% select(district,no_of_women_reached) %>%
                arrange(desc(no_of_women_reached)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,no_of_women_reached),y=no_of_women_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_women_reached))+
                labs(
                    title = "Top 5 counties",
                    y="Number of women reached",
                    x="District"
                )+
                coord_flip()
            
        }  
        
        else if("6" %in% input$stats){
            zwe_data %>% select(district,no_of_men_reached) %>%
                arrange(desc(no_of_men_reached)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,no_of_men_reached),y=no_of_men_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_men_reached))+
                labs(
                    title = "Top 5 counties",
                    y="Number of men reached",
                    x="District"
                )+
                coord_flip()
            
        }  
        
        else if("7" %in% input$stats){
            zwe_data %>% select(district,intial_4w_recotd) %>%
                arrange(desc(intial_4w_recotd)) %>%
                top_n(5) %>% ggplot(aes(x=reorder(district,intial_4w_recotd),y=intial_4w_recotd))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=intial_4w_recotd))+
                labs(
                    title = "Top 5 counties",
                    y="Initial recorded average number",
                    x="District"
                )+
                coord_flip()
            
        }  
        
        
    })
    
    
    output$bottom<-renderPlotly({
        if("1" %in% input$stats){
            zwe_data %>% select(district,no_of_individual_targeted) %>%
                arrange(desc(no_of_individual_targeted)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,no_of_individual_targeted),y=no_of_individual_targeted))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_individual_targeted))+
                labs(
                    title = "Bottom 5 counties",
                    y="Total Population",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("2" %in% input$stats){
            zwe_data %>% select(district,total_no_of_individuals_reached) %>%
                arrange(desc(total_no_of_individuals_reached)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,total_no_of_individuals_reached),y=total_no_of_individuals_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=total_no_of_individuals_reached))+
                labs(
                    title = "Bottom 5 counties",
                    y="Population Density",
                    x="District"
                )+
                coord_flip()
        }
        
        else if("3" %in% input$stats){
            zwe_data %>% select(district,no_of_boys_reached) %>%
                arrange(desc(no_of_boys_reached)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,no_of_boys_reached),y=no_of_boys_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_boys_reached))+
                labs(
                    title = "Bottom 5 counties",
                    y="Number of boys",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("4" %in% input$stats){
            zwe_data %>% select(district,no_of_girls_reached) %>%
                arrange(desc(no_of_girls_reached)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,no_of_girls_reached),y=no_of_girls_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_girls_reached))+
                labs(
                    title = "Bottom 5 counties",
                    y="FeNumber of boys",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("5" %in% input$stats){
            zwe_data %>% select(district,no_of_women_reached) %>%
                arrange(desc(no_of_women_reached)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,no_of_women_reached),y=no_of_women_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_women_reached))+
                labs(
                    title = "Bottom 5 counties",
                    y="Number of women reached",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("6" %in% input$stats){
            zwe_data %>% select(district,no_of_men_reached) %>%
                arrange(desc(no_of_men_reached)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,no_of_men_reached),y=no_of_men_reached))+
                geom_col(fill="#FF6666")+
                geom_text(aes(label=no_of_men_reached))+
                labs(
                    title = "Bottom 5 counties",
                    y="Number of men reached",
                    x="District"
                )+
                coord_flip()
            
        }
        
        else if("7" %in% input$stats){
            zwe_data %>% select(district,intial_4w_recotd) %>%
                arrange(desc(intial_4w_recotd)) %>%
                top_n(-5) %>% ggplot(aes(x=reorder(district,intial_4w_recotd),y=intial_4w_recotd))+
                geom_col()+
                geom_text(aes(label=intial_4w_recotd))+
                labs(
                    title = "Bottom 5 counties",
                    y="Initial recorded average number",
                    x="District"
                )+
                coord_flip()
            
        }  
        
    })
    
    
    
    output$table<-renderDataTable({
        datatable(zwe_data,
                  class = 'cell-border stripe',
                  editable = TRUE,
                  options = list(scrollX = T)
        ) 
    }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)


