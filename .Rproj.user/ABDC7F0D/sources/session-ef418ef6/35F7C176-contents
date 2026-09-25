# Librerías

library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(rsconnect)
library(RColorBrewer)
library(readxl)
library(shinyBS)
library(shinyWidgets)

tasas_swap <- read_xlsx("datos.xlsx")

# Shiny

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    
    body, .container-fluid, .main-panel {
      background-color: transparent !important;
      margin: 0 !important;
      padding-bottom: 0 !important;
    }
    
  "))
  ),
  
  tags$h4(
    "Swaps Promedio Cámara ",
    tags$span(
      icon("question-circle"), 
      id = "info_icon", 
      style = "cursor: pointer; color: #007BFF;"
    )
  ),
  bsTooltip(
    id = "info_icon",
    title = paste(
      "El Swap Promedio Cámara (SPC) es un derivado que combina una tasa fija pactada con una tasa variable.",
      "Dado que los swaps son instrumentos altamente transados, sirven como referencia para identificar",
      "expectativas sobre la política monetaria del Banco Central, mientras que el",
      "diferencial entre pesos y UF (compensación inflacionaria) suele ser análogo a las expectativas de inflación."
    ),
    placement = "right",
    trigger = "click"
  ),
  
  tags$p("Por: Benjamín Pescio", style = "font-size: 12px; margin: 1;"),
  
  div(class = "radio-group-custom",
      prettyRadioButtons(
        inputId = "denominacion",
        label = NULL,
        choices = c(
          "SPC en pesos" = "CLP",
          "SPC en UF" = "UF",
          "Compensación inflacionaria" = "Compensación inflacionaria"
        ),
        selected = "CLP",
        inline = TRUE,
        animation = "smooth",
        status = "primary",
        shape = "round",
        outline = TRUE
      )
  ),
  
  fluidRow(
    column(width = 12,
           div(
             id = "grafico-container",
             class = "fade-in",
             style = "position: relative; left: -20px;",
             plotlyOutput("grafico", height = "300px", width = "100%")
           ),
           tags$div(
             style = "display: flex; justify-content: space-between; align-items: center;
                    margin-top: 10px;",
           tags$div(
             style = "font-size: 11px; color: black; display: flex; flex-direction: column; align-items: flex-start;",
             tags$img(src = "icono_flecha.svg", height = "30px", style = "margin-bottom: 2px;"),
             "Fuente: Banco Central"
             ),
           tags$img(src = "footer.png", height = "35px")
           )
           )
    )
  )

server <- function(input, output, session) {
  
  output$grafico <- renderPlotly({
    
    colores <- if (input$denominacion == "CLP") {
      c("#FFEA6C", "#FF5500")
    } else if (input$denominacion == "UF") {
      c("#CCD67F", "#118B50")
    } else {
      c("#DFF1F1", "#088395")
    }

    filtrado <- tasas_swap %>%
      filter(Categoria == input$denominacion) %>%
      select(-Categoria)
    
    series <- unique(filtrado$Serie)
    
    filtrado <- filtrado %>%
      pivot_wider(values_from="Valor", names_from="Serie") %>%
      na.locf() %>%
      pivot_longer(cols=-Fecha, values_to="Valor", names_to="Serie")
    
    fecha_max <- filtrado %>%
      filter(Fecha == max(Fecha)) %>%
      pull(Fecha) %>%
      unique()
    
    cierre_anio_anterior <- paste0(
      year(fecha_max) - 1, "-12-31"
    )
    
    seleccion_fechas <- c(fecha_max, cierre_anio_anterior)
    
    pre_curva_1 <- filtrado %>%
      filter(Fecha %in% seleccion_fechas) %>%
      arrange(Fecha, Serie)
    
    pre_curva_2 <- pre_curva_1 %>%
      mutate(
        Serie = factor(Serie, levels = series),
        niveles_fecha = factor(Fecha, levels = unique(pre_curva_1$Fecha))
        )
    
    niveles_fecha <- sort(unique(pre_curva_1$Fecha))
    etiquetas_fechas <- format(niveles_fecha, "%d-%m-%Y")
    paleta <- colorRampPalette(c(colores[1], tail(colores, 1)))(length(niveles_fecha))
    paleta_named <- setNames(paleta, etiquetas_fechas)
    
    curva <- pre_curva_1 %>%
      mutate(
        Serie      = factor(Serie, levels = series),
        fecha_lbl = factor(format(Fecha, "%d-%m-%Y"),
                           levels = etiquetas_fechas)
      ) %>%
      arrange(Serie)

    grafico <- plot_ly(
      data    = curva,
      x       = ~Serie,
      y       = ~Valor,
      split   = ~fecha_lbl,
      color   = ~fecha_lbl,
      colors  = paleta_named,
      type    = "scatter",
      mode    = "lines+markers",
      line    = list(width = 3),
      marker  = list(size = 10, symbol = "diamond"),
      hovertemplate = paste0(
        "%{fullData.name}: %{y:.2f}%<extra></extra>"
      ),
      text = ~fecha_lbl
    ) %>%
      plotly::layout(
        dragmode = FALSE,
        hovermode = "x unified",
        xaxis = list(title = ""),
        yaxis = list(title = "Tasa de interés (%)"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(
          orientation = "h",
          x = 0, xanchor = "left",
          font = list(size = 9),
          traceorder = "normal"     # respeta el orden de levels()
        )
      ) %>%
      plotly::config(
        displayModeBar = FALSE,
        showTips = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        displaylogo = FALSE,
        responsive = TRUE,
        locale = "es"
      )
  })
}

shinyApp(ui, server)
