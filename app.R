library(shiny)
library(PerformanceAnalytics)
library(argonR)
library(dplyr)
library(argonDash)
library(DT)
library(echarts4r)
library(quantmod)

# App
ui <- argonDashPage(
    title = "App de seguimiento de Stocks",
    author = "Jorge Hernández",
    description = "App de seguimiento de Stocks",
    
    sidebar = argonDashSidebar(
        vertical = TRUE,
        skin = "dark",
        background = "dark",
        size = "lg",
        side = "left",
        id = "my_sidebar",
        brand_url = "https://r-conomics.netlify.app",
        brand_logo = "https://r-conomics.netlify.app/images/perro1.png",
        
        argonSidebarHeader(title = "Menu Principal"),
        argonSidebarMenu(
            argonSidebarItem(
                tabName = "home_tab",
                style="text-align:center",
                icon = argonIcon("chart-bar-32"),
                "Gráficos"
            ),
            argonSidebarItem(
                tabName = "stats_tab",
                style="text-align:center",
                icon = argonIcon("cloud-download-95"),
                "Datos"
            ),
            hr(),
            tags$style('.input-sm {font-size: 12px; } label {font-weight: 300; margin-bottom: 10px; }'),
            dateInput("fecha1", "Fecha inicial", value = "2015-01-01"),
            dateInput("fecha2", "Fecha final", value = "2017-01-01"),
            textInput("Stock","Ingresa un stock"),
            actionButton("GO","Obtener info")),
        argonSidebarDivider(),
        argonSidebarHeader(title = "@Versión 0.1")
    ),
    
    navbar = argonDashNavbar(
        argonDropNav(
            title = "Hecho por Jorge Hernández",
            # src = "https://demos.creative-tim.com/argon-dashboard/assets/img/theme/team-4-800x800.jpg",
            src = "https://cdn.pixabay.com/photo/2016/06/22/14/07/logo-1473038_1280.png",
            orientation = "right",
            argonDropNavTitle(title = "Mira mis otros proyectos"),
            argonDropNavItem(
                title = "Destacados",
                src = "https://r-conomics.netlify.app/course/",
                icon = argonIcon("book-bookmark")
            ),
            argonDropNavItem(
                title = "Tutoriales R",
                src = "https://r-conomics.netlify.app/blog/",
                icon = argonIcon("folder-17")
            ),
            argonDropNavDivider(),
            argonDropNavItem(
                title = "Mi CV",
                src = "https://r-conomics.netlify.app/images/curriculum.pdf",
                icon = argonIcon("briefcase-24")
            )
        )
    ),
    
    header = argonDashHeader(
        gradient = TRUE,
        color = "default",
        separator = TRUE,
        separator_color = "secondary"
    ),
    
    body = argonDashBody(
        argonTabItems(
            argonTabItem(
                tabName = "home_tab",
                fluidRow(
                    argonCard(
                        status = "default",
                        width = 12,
                        title = "Gráfica de la serie",
                        hover_lift = FALSE,
                        shadow = TRUE,
                        icon = argonIcon("chart-pie-35"),
                        #src = "#",
                        echarts4rOutput("graf1")
                    ),
                    argonCard(
                        status = "default",
                        width = 6,
                        title = "Máximos y mínimos",
                        hover_lift = FALSE,
                        shadow = TRUE,
                        icon = argonIcon("bold-up"),
                        #src = "#",
                        echarts4rOutput("graf2")
                    ),
                    argonCard(
                        status = "default",
                        width = 6,
                        title = "Información adicional",
                        hover_lift = FALSE,
                        shadow = TRUE,
                        icon = argonIcon("money-coins"),
                        #src = "#",
                        textOutput( "stats" )
                    ) ) ),
            argonTabItem(
                tabName = "stats_tab",
                argonCard(
                    status = "primary",
                    width = 12,
                    title = "Los datos descargados de la serie",
                    hover_lift = FALSE,
                    shadow = TRUE,
                    icon = argonIcon("cloud-download-95"),
                    #src = "#",
                    dataTableOutput("table")
                )
                
            )
        )
    ),
    footer = argonDashFooter(
        copyrights = "@Jorge Hernández, 2021",
        src = "https://www.linkedin.com/in/jorge-hernandez98/",
        argonFooterMenu(
            argonFooterItem("Linkedin", src = "https://www.linkedin.com/in/jorge-hernandez98/"),
            argonFooterItem("Github", src = "https://github.com/Jorge-hercas"),
            argonFooterItem("R-conomics", src = "https://r-conomics.netlify.app"),
            argonFooterItem("Instagram", src = "https://www.instagram.com/jorge_hca/")
        )
    )
    
)

server <- function(input, output) {
    
    data <- eventReactive(input$GO,{
        req(input$Stock)
        getSymbols(input$Stock,src = "yahoo", from= input$fecha1 ,to= input$fecha2 ,auto.assign=F)
    })
    
    datos <- reactive({data.frame(Fecha =  as.Date(index(data())), data()     ) |> 
            na.omit()
        
        
        
        })
    
    
    output$table <- renderDataTable({
        datos()
    })
    
    
    grafico <- reactive({
        
        datos() |>
            rename( simbolo = names(datos()[5])   )
        
    })
    
    grafico1 <- reactive({
        
        grafico() |>
            filter(simbolo == max(simbolo) | simbolo == min(simbolo)  )
        
    })
    
    
    
    
    output$graf1 <- renderEcharts4r({
        
        grafico() |>
            e_charts(Fecha) |>
            e_line(simbolo, symbol = "none", name = input$Stock) |>
            e_tooltip(trigger = "axis") |>
            e_datazoom() |>
            e_y_axis(min = min(grafico()$simbolo)) |>
            e_color(color = "#8a3333") |>
            e_toolbox_feature(
                feature = "magicType",
                type = list("line", "bar")
            ) |>
            e_brush() |>
            e_toolbox_feature(feature = "saveAsImage") |>
            e_toolbox_feature(feature = "restore")
        
    })
    
    output$graf2 <- renderEcharts4r({
        
        grafico1() |>
            e_charts(Fecha) |>
            e_bar(simbolo, symbol = "none", name = input$Stock) |>
            e_tooltip(trigger = "axis") |>
            #e_color(color = "#8a3333") |>
            #e_brush() |>
            e_toolbox_feature(feature = "saveAsImage")
        #e_toolbox_feature(feature = "restore")
        
    })
    output$stats <- renderText( print(paste(  "El máximo observado en la serie fue de una cotización de",
                                              round(max(grafico()[,5]), digits = 2), "durante el periodo", 
                                              grafico()$Fecha[which.max(grafico()$simbolo  )], 
                                              "mientras que el valor mínimo observado fue una cotización de",
                                              round(min(grafico()[,5]), digits = 2), "durante el periodo",
                                              grafico()$Fecha[which.min(grafico()$simbolo  )], ".", 
                                              "Por otra parte, la serie presenta una varianza igual a",
                                              round(var(grafico()[,5]), digits = 2), "y una cotización promedio igual a",
                                              round(mean(grafico()[,5]), digits = 2)
                                              
                                              
    )  )  )
    
}

shinyApp(ui, server)










