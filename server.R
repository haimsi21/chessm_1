

server <- function(input, output, session) {
  ### Inicio
  output$greeting <- renderText({
    paste(
      "Bienvenido al futuro donde las máquinas siguen tus órdenes,",
      input$nam," ",input$sur)
  })
  
  
  output$onum <- renderText(input$num)
  
  df1<-reactive({ 
    f_df1<-
    df %>%
      filter(x <= input$num)
  })
  
  output$p <- renderPlotly({
    
    plot_ly(
      df1(),
      type = "scatterpolargl",
      mode = "markers",
      opacity = input$tra,
      height = input$alt,
      width = input$anh,
      color = ~x,
      showlegend = FALSE  # Oculta la leyenda
    ) %>%
      add_trace(
        r = ~get(input$irra),
        theta = ~x,
        name = input$irra,
        marker = list(size = input$tam, colorscale = "Viridis")
      ) %>%
      layout(
        polar = list(
          radialaxis = list(showticklabels = FALSE, showline = FALSE, showgrid = FALSE),  # Elimina los números, la línea y la cuadrícula del eje radial
          angularaxis = list(showticklabels = FALSE, showline = FALSE, showgrid = FALSE)  # Elimina los números, la línea y la cuadrícula del eje angular
        ),
        xaxis = list(showgrid = FALSE, zeroline = FALSE),  # Elimina la cuadrícula y la línea cero en el eje X
        yaxis = list(showgrid = FALSE, zeroline = FALSE),  # Elimina la cuadrícula y la línea cero en el eje Y
        margin = list(r = 0, l = 0, t = 0, b = 0),  # Ajusta los márgenes para eliminar el espacio adicional
        plot_bgcolor = "rgba(0,0,0,0)",  # Define un color de fondo transparente
        paper_bgcolor = "rgba(0,0,0,0)"  # Define un color de fondo transparente para el área de la gráfica
      )
  })
  
  df2<-reactive({ 
    f_df2<-
      df %>%
      filter(x <= input$ncc)
  })
  output$cm <- renderPlotly({
    
    plot_ly(
      data = df2(),
      x =  ~ fi,
      y =  ~ exm,
      z =  ~ pim,
      opacity = input$tra1,
      type = "scatter3d",
      mode = "markers",
      hoverinfo = "skip",
      marker = list(size = input$siz,
                    color = ~x, 
                    colorscale = "Viridis"),
      width = input$anh1,
      height = input$alt1
      #colors =  c(input$col1, input$col2)
    ) %>%
      hide_colorbar() %>%
      layout(scene = list(
        xaxis = list(
          visible = F,
          showgrid = F,
          showline = F,
          zeroline = F
        ),
        yaxis = list(
          visible = F,
          showgrid = F,
          showline = F,
          zeroline = F
        ),
        zaxis = list(visible = F)
      )) %>%
      config(displaylogo = F, displayModeBar = T)
    
  })
  
  df3<-reactive({ 
    f_df3<-
      df %>%
      filter(x <= input$n2d)
     })
  
  output$hex2d <- renderPlotly({
    
    fig <- plot_ly(df3(),x = ~get(input$irra2d), y = ~get(input$irra2dy))
    fig2 <- subplot(
      fig %>% add_trace(alpha = 0.3),
      fig %>% add_histogram2d(colorscale = "Viridis")
    )
    
  })
}

shinyApp(ui, server)
