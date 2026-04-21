

###############################DEPLOYMENT START######################################


library(shiny)
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Reporte de Activos"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Cargar archivo Excel", accept = c(".xlsx", ".xls")),
      dateInput("fecha_inicio", "Fecha inicio", value = as.Date("2023-06-01")),
      dateInput("fecha_corte_inst", "Fecha corte instalación", value = as.Date("2024-01-31")),
      dateInput("fecha_corte_desinst", "Excluir desinstalados hasta", value = as.Date("2026-01-31"))
    ),
    
    mainPanel(
      h3("Tabla de resultados"),
      DTOutput("tabla_resultado"),
      br(),
      h3("Gráfico de instalaciones por mes"),
      plotOutput("grafico"),
      br(),
      h3("Tabla de datos del gráfico"),
      DTOutput("tabla_grafico")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactive({
    req(input$archivo)
    
    df <- read_excel(input$archivo$datapath)
    
    df$`Fecha Instalacion` <- as.Date(df$`Fecha Instalacion`, format = "%d/%m/%Y")
    df$`Fecha Desinstalacion` <- as.Date(df$`Fecha Desinstalacion`, format = "%d/%m/%Y")
    
    df
  })
  
  resultado_filtrado <- reactive({
    df <- datos()
    
    df %>%
      filter(
        `Fecha Instalacion` >= input$fecha_inicio &
          `Fecha Instalacion` <= input$fecha_corte_inst &
          (
            is.na(`Fecha Desinstalacion`) |
              `Fecha Desinstalacion` > input$fecha_corte_desinst
          )
      ) %>%
      select(
        Nombres,
        Apellidos,
        `Nombre Completo`,
        `Fecha Instalacion`,
        `Fecha Desinstalacion`,
        Estado,
        `Estado Monitoreo`
      )
  })
  
  datos_grafico <- reactive({
    df <- resultado_filtrado()
    
    df %>%
      mutate(mes = floor_date(`Fecha Instalacion`, "month")) %>%
      group_by(mes) %>%
      summarise(instalados = n(), .groups = "drop")
  })
  
  output$tabla_resultado <- renderDT({
    datatable(
      resultado_filtrado(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$grafico <- renderPlot({
    ggplot(datos_grafico(), aes(x = mes, y = instalados)) +
      geom_line() +
      geom_point() +
      labs(
        title = "Instalaciones por mes",
        x = "Mes",
        y = "Cantidad"
      )
  })
  
  output$tabla_grafico <- renderDT({
    datatable(
      datos_grafico(),
      colnames = c("Mes", "Cantidad de instalaciones"),
      options = list(pageLength = 12),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)