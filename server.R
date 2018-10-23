library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(devtools)

get_column_name <-
  function(names, choice, datapath, type = "guess") {
    i_col_choice <- isolate(choice)
    i_col_names <- isolate(names)
    col_type <- rep("skip", length(i_col_names))
    n_col_seq <- match(i_col_choice, i_col_names)
    col_type[n_col_seq] <- type
    
    seqce <-
      read_excel(
        datapath,
        sheet = 1,
        col_names = TRUE,
        col_types = col_type
      )
    
    return(seqce)
  }

create_plot <- function(table,
                        text,
                        seq_name = "Your Sequence",
                        plot_mode = 'lines+markers',
                        average = Aver_plt,
                        sigma = Sigm_plt,
                        tool_mode = tool_mod,
                        plot_title = plt_title,
                        x_label = x_lbl,
                        y_label = y_lbl) {
  p <- plot_ly(
    name = seq_name,
    data = table,
    type = 'scatter',
    mode = plot_mode,
    text = text,
    hoverinfo  = 'text+name',
    x = ~ point,
    y = ~ value
  )
  p <- layout(
    p = p,
    title = plot_title,
    legend = list(orientation = 'h'),
    xaxis = list(title = x_label,
                 type = 'category'),
    yaxis = list(title = y_label)
  )
  
  if (tool_mode == 'plot+srule') {
    line_value <- c(average - 3 * sigma, average, average + 3 * sigma)
    line_name <- c("LCL", "Av", "UCL")
    line_color <- c("red", "green", "red")
    
    bands_colors <- c('#FCBBA1',
                      '#FFFFBF',
                      '#C7E9C0',
                      '#C7E9C0',
                      '#FFFFBF',
                      '#FCBBA1')
    
    annot <- c(
      paste("LCL: ", round(line_value[1], 4)),
      paste("Aver: ", round(line_value[2], 4), "   Sigm: ", round(sigma, 4)),
      paste("UCL: ", round(line_value[3], 4))
    )
    
    for (i in 1:3) {
      p <- add_lines(
        p = p,
        y = line_value[i],
        name = line_name[i],
        type = 'scatter',
        hoverinfo = 'none',
        showlegend = FALSE,
        color = I(line_color[i])
      )
      
      line_label <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.05,
        y = line_value[i],
        xanchor = 'left',
        yanchor = 'bottom',
        text = paste("<b>", annot[i], "</b>"),
        font = list(family = 'sans-serif',
                    size = 14),
        showarrow = FALSE
      )
      
      p <- layout(p = p,
                  annotations = line_label)
      
    }
    
    for (i in 1:6) {
      p <- add_ribbons(
        p = p,
        ymin = (average - 3 * sigma) + (i - 1) * sigma,
        ymax = (average - 3 * sigma) + i * sigma,
        color = I(bands_colors[i]),
        hoverinfo = 'none',
        showlegend = FALSE,
        line = list(width = 0)
      )
    }
    
  }
  return(p)
}

x_lbl <<- "Point"
y_lbl <<- "Value"
plt_title <<- "Your plot"
tool_mod <<- "plot"
read_seq <<- NULL
Aver_plt <<- NULL
Sigm_plt <<- NULL

server <- function(input, output, session) {
  # Получение названий столбцов для выбора
  data <- reactive({
    if (is.null(input$file1))
      return(NULL)
    read_excel(
      input$file1$datapath,
      sheet = 1,
      col_names = TRUE,
      n_max = 0
    )
  })
  
  col_names <- reactive({
    colnames(data())
  })
  
  output$vars <- renderUI({
    if (is.null(data()))
      return(NULL)
    tagList(radioButtons("show_vars", "Choose sequence:",
                         col_names()))
  })
  # ---
  col_choice <- reactive({
    input$show_vars
  })
  
  # Выгрузка выбранных столбцов
  
  observeEvent(input$file1, {
    insertUI("#show_vars", "afterEnd",
             tagList(actionButton("form_table", "Confirm"),
                     hr()))
    
  })
  
  observeEvent(input$form_table, {
    if (is.null(read_seq)) {
      read_seq <<- get_column_name(col_names(), col_choice(), input$file1$datapath)
      num_seq <<- read_seq[[1]]
      data_seq <<- tibble(point = 1:length(num_seq), value = num_seq)
    } else{
      xaxis_val <<- get_column_name(col_names(), col_choice(), input$file1$datapath)
      s_seq <<- xaxis_val[[1]]
      data_seq <<- tibble(point = s_seq, value = num_seq)
    }
    
    
    point_info <<-
      paste("Point: ", data_seq$point, "<br>", "Value: ",
            data_seq$value)
    
    tool_mod <<- tool_mod
    
    output$tbl <- renderDataTable({
      return(data_seq)
    })
    
    output$plotly <- renderPlotly({
      create_plot(table = data_seq, text = point_info)
    })
    
    hide(id = "vars")
    
    
  })
  observeEvent(input$add_shewhart, {
    # devtools::install("ShewhartQCC")
    # library(ShewhartQCC)
    # Sigm_plt <<- sd(num_seq, na.rm = TRUE)
    # Aver_plt <<- mean(num_seq, na.rm = TRUE)
    # trash_1 <<- Shewchart_1(data_seq, data_seq$value, Aver_plt + 3*Sigm_plt,  Aver_plt -3*Sigm_plt)
    # trash_2 <<- Shewchart_2(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    # trash_3 <<- Shewchart_3(data_seq, data_seq$value)
    # trash_4 <<- Shewchart_4(data_seq, data_seq$value)
    # trash_5 <<- Shewchart_5(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    # trash_6 <<- Shewchart_6(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    # trash_7 <<- Shewchart_7(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    # trash_8 <<- Shewchart_8(data_seq, data_seq$value, Aver_plt, Sigm_plt)
  })
  
  shinyjs::onclick("form_table",
                   shinyjs::show(id = "plottools", anim = TRUE))
  
  shinyjs::onclick("toggle_advanced", {
    shinyjs::toggle(id = "tools", anim = TRUE)
    shinyjs::hide(id = "layout", anim = TRUE)
  })
  
  shinyjs::onclick("labels",
                   shinyjs::toggle(id = "layout", anim = TRUE))
  
  observeEvent(input$get_label, {
    x_lbl <<- input$x_label
    y_lbl <<- input$y_label
    plt_title <<- input$plot_title
    
    output$plotly <- renderPlotly({
      create_plot(table = data_seq, text = point_info)
    })
  })
  
  
  observeEvent(input$show_x_val, {
    show(id = "vars")
  })
  
  
  observeEvent(input$sigm, {
    Sigm_plt <<- sd(num_seq, na.rm = TRUE)
    Aver_plt <<- mean(num_seq, na.rm = TRUE)
    tool_mod <<- "plot+srule"
    
    output$plotly <- renderPlotly({
      create_plot(table = data_seq, text = point_info)
    })
  })
  #--- Конец сервера
}
