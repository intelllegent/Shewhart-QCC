library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(devtools)


s_Sequence_up <- function(sample, operand){
  l_Operand <- sample > operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

s_Sequence_low <- function(sample, operand){
  l_Operand <- sample < operand
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

s_Sequence_in <- function(sample, operand1, operand2){
  if (operand1 > operand2){
    buf <- operand1
    operand1 <- operand2
    operand2 <- buf
  }
  l_Operand1 <- sample > operand1
  l_Operand2 <- sample < operand2
  l_Operand <- l_Operand1 & l_Operand2
  b_Operand <- 1 * l_Operand
  fin <- paste0(b_Operand, collapse = "")
  return(fin)
}

s_Difference <-  function(sample){
  v_diff <- diff(sample)
  l_diff <- v_diff > 0
  b_diff <- 1 * l_diff
  fin <- paste0(b_diff, collapse = "")
  return(fin)
}

v_Position <- function (exp, string){
  list_pos <- gregexpr(exp, string)
  v_pos <- unlist(list_pos)
  return(v_pos)
}


Shewchart_1 <- function(table, sample, ucl, lcl) {
  data_UCL <- NULL
  data_LCL <- NULL
  
  if (lcl > ucl) {
    buf <-  lcl
    lcl <-  ucl
    ucl <-  buf
  }
  
  s_UCL <- s_Sequence_up(sample, ucl)
  s_LCL <- s_Sequence_low(sample, lcl)
  
  id_UCL <- v_Position("1", s_UCL)
  if (length(id_UCL) != 1) {
    data_UCL <- table[id_UCL, ]
  } else if (id_UCL != -1) {
    data_UCL <- table[id_UCL, ]
  }
  
  
  id_LCL <- v_Position("1", s_LCL)
  if (length(id_LCL) != 1) {
    data_LCL <- table[id_LCL, ]
  } else if (id_LCL != -1) {
    data_LCL <- table[id_LCL, ]
  }
  
  Fin <- do.call(rbind, list(data_LCL, data_UCL))
  return(Fin)
}

Shewchart_2 <- function(table, sample, average, sigma) {
  data_zone_C <- NULL
  data_up <- NULL
  data_low <- NULL
  data_buff <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma
  
  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111", s_zone_C)
  
  if (length(id_zone_C) != 1) {
    for (i in 1:length(id_zone_C)){
      data_buff <- table[id_zone_C[i]:(id_zone_C[i] + 8), ]
      data_zone_C <- do.call(rbind, list(data_zone_C, data_buff))
    }
  } else if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 8), ]
  }
  
  s_upper_average <- s_Sequence_up(sample, average)
  id_up <- v_Position("111111111", s_upper_average)
  
  if (length(id_up) != 1) {
    for (i in 1:length(id_up)){
      data_buff <- table[id_up[i]:(id_up[i] + 8), ]
      data_up <- do.call(rbind, list(data_up, data_buff))
    }
  } else if (id_up != -1) {
    data_up <- table[id_up:(id_up + 8), ]
  }
  
  id_low <- v_Position("000000000", s_upper_average)
  if (length(id_low) != 1) {
    for (i in 1:length(id_low)){
      data_buff <- table[id_low[i]:(id_low[i] + 8), ]
      data_low <- do.call(rbind, list(data_low, data_buff))
    }
  } else if (id_low != -1) {
    data_low <- table[id_low:(id_low + 8), ]
  }
  
  Fin <- do.call(rbind, list(data_zone_C, data_up, data_low))
  return(unique(Fin))
}

Shewchart_3 <- function(table, sample) {
  data_buff <- NULL
  data_incr <- NULL
  data_decr <- NULL
  incr_decr <- NULL
  s_diff <- s_Difference(sample)
  
  id_diff_incr <- v_Position("11111", s_diff)
  if (length(id_diff_incr) != 1) {
    for (i in 1:length(id_diff_incr)){
      data_buff <- table[(id_diff_incr[i] - 1):(id_diff_incr[i] + 6), ]
      data_incr <- do.call(rbind, list(data_incr, data_buff))
    }
  } else if (id_diff_incr != -1) {
    data_incr <- table[(id_diff_incr - 1):(id_diff_incr + 6), ]
  }
  
  
  id_diff_decr <- v_Position("00000", s_diff)
  if (length(id_diff_decr) != 1) {
    for (i in 1:length(id_diff_decr)){
      data_buff <- table[(id_diff_decr[i] - 1):(id_diff_decr[i] + 6), ]
      data_decr <- do.call(rbind, list(data_decr, data_buff))
    }
  } else if (id_diff_decr != -1) {
    data_decr <- table[(id_diff_decr - 1):(id_diff_decr + 6), ]
  }
  
  incr_decr <- do.call(rbind, list(data_incr, data_decr))
  
  Fin <- incr_decr
  return(unique(Fin))
}

Shewchart_4 <- function(table, sample) {
  data_in_dec <- NULL
  data_dec_in <- NULL
  data_buff <- NULL
  s_diff <- s_Difference(sample)
  
  id_in_dec <- v_Position("1010101010101", s_diff)
  if (length(id_in_dec) != 1) {
    for (i in 1:length(id_in_dec)){
      data_buff <- table[(id_in_dec[i] - 1):(id_in_dec[i] + 14), ]
      data_in_dec <- do.call(rbind, list(data_in_dec, data_buff))
    }
  } else if (id_in_dec !=  -1) {
    data_in_dec <- table[(id_in_dec - 1):(id_in_dec + 14), ]
  }
  
  id_dec_in <- v_Position("0101010101010", s_diff)
  if (length(id_dec_in) != 1) {
    for (i in 1:length(id_dec_in)){
      data_buff <- table[(id_dec_in[i] - 1):(id_dec_in[i] + 14), ]
      data_dec_in <- do.call(rbind, list(data_dec_in, data_buff))
    }
  } else if (id_dec_in !=  -1) {
    data_dec_in <- table[(id_dec_in-1):(id_dec_in + 14), ]
  }
  
  Fin <- do.call(rbind, list(data_in_dec, data_dec_in))
  
  return(unique(Fin))
}

Shewchart_5 <- function(table, sample, average, sigma) {
  data_buff <- NULL
  data_zone_A_up <- NULL
  data_zone_A_low <- NULL
  data_zone_A_up_2 <- NULL
  data_zone_A_low_2 <- NULL
  
  s_zone_A_up <- s_Sequence_up(sample, average + 2 * sigma)
  s_zone_A_low <- s_Sequence_low(sample, average - 2 * sigma)
  
  id_zone_A_up <- v_Position("11", s_zone_A_up)
  if (length(id_zone_A_up) != 1) {
    for (i in 1:length(id_zone_A_up)){
      data_buff <- table[id_zone_A_up[i]:(id_zone_A_up[i] + 2), ]
      data_zone_A_up <- do.call(rbind, list(data_zone_A_up, data_buff))
    }
  } else if (id_zone_A_up != -1) {
    data_zone_A_up <- table[id_zone_A_up:(id_zone_A_up + 2), ]
  }
  
  id_zone_A_low <- v_Position("11", s_zone_A_low)
  if (length(id_zone_A_low) != 1) {
    for (i in 1:length(id_zone_A_low)){
      data_buff <- table[id_zone_A_low[i]:(id_zone_A_low[i] + 2), ]
      data_zone_A_low <- do.call(rbind, list(data_zone_A_low, data_buff))
    }
  } else if (id_zone_A_low != -1) {
    data_zone_A_low <- table[id_zone_A_low:(id_zone_A_low + 2), ]
  }
  
  id_zone_A_up_2 <- v_Position("101", s_zone_A_up)
  if (length(id_zone_A_up_2) != 1) {
    for (i in 1:length(id_zone_A_up_2)){
      data_buff <- table[id_zone_A_up_2[i]:(id_zone_A_up_2[i] + 2), ]
      data_zone_A_up_2 <- do.call(rbind, list(data_zone_A_up_2, data_buff))
    }
  } else if (id_zone_A_up_2 != -1) {
    data_zone_A_up_2 <- table[id_zone_A_up_2:(id_zone_A_up_2 + 2), ]
  }
  
  id_zone_A_low_2 <- v_Position("101", s_zone_A_low)
  if (length(id_zone_A_low_2) != 1) {
    for (i in 1:length(id_zone_A_low_2)){
      data_buff <- table[id_zone_A_low_2[i]:(id_zone_A_low_2[i] + 2), ]
      data_zone_A_low <- do.call(rbind, list(data_zone_A_low_2, data_buff))
    }
  } else if (id_zone_A_low_2 != -1) {
    data_zone_A_low_2 <- table[id_zone_A_low_2:(id_zone_A_low_2 + 2), ]
  }
  
  Fin <-
    do.call(
      rbind,
      list(
        data_zone_A_up,
        data_zone_A_low ,
        data_zone_A_up_2,
        data_zone_A_low_2
      )
    )
  return(unique(Fin))
}

Shewchart_6 <- function(table, sample, average, sigma) {
  data_buff <- NULL
  data_zone_B_up <- NULL
  data_zone_B_up_2 <- NULL
  data_zone_B_up_3 <- NULL
  data_zone_B_up_4 <- NULL
  data_zone_B_low <- NULL
  data_zone_B_low_2 <- NULL
  data_zone_B_low_3 <- NULL
  data_zone_B_low_4 <-  NULL
  
  s_zone_B_up <- s_Sequence_up(sample, average + sigma)
  s_zone_B_low <- s_Sequence_low(sample, average - sigma)
  
  id_zone_B_up <- v_Position("1111", s_zone_B_up)
  if (length(id_zone_B_up) != 1) {
    for (i in 1:length(id_zone_B_up)){
      data_buff <- table[id_zone_B_up[i]:(id_zone_B_up[i] + 4), ]
      data_zone_B_up <- do.call(rbind, list(data_zone_B_up, data_buff))
    }
  } else if (id_zone_B_up != -1) {
    data_zone_B_up <- table[id_zone_B_up:(id_zone_B_up + 4), ]
  }
  
  id_zone_B_up_2 <- v_Position("11101", s_zone_B_up)
  if (length(id_zone_B_up_2) != 1) {
    for (i in 1:length(id_zone_B_up_2)){
      data_buff <- table[id_zone_B_up_2[i]:(id_zone_B_up_2[i] + 4), ]
      data_zone_B_up_2 <- do.call(rbind, list(data_zone_B_up_2, data_buff))
    }
  } else if (id_zone_B_up_2 != -1) {
    data_zone_B_up_2 <- table[id_zone_B_up_2:(id_zone_B_up_2 + 4), ]
  }
  
  id_zone_B_up_3 <- v_Position("11011", s_zone_B_up)
  if (length(id_zone_B_up_3) != 1) {
    for (i in 1:length(id_zone_B_up_3)){
      data_buff <- table[id_zone_B_up_3[i]:(id_zone_B_up_3[i] + 4), ]
      data_zone_B_up_3 <- do.call(rbind, list(data_zone_B_up_3, data_buff))
    }
  } else if (id_zone_B_up_3 != -1) {
    data_zone_B_up_3 <- table[id_zone_B_up_3:(id_zone_B_up_3 + 4), ]
  }
  
  id_zone_B_up_4 <- v_Position("10111", s_zone_B_up)
  if (length(id_zone_B_up_4) != 1) {
    for (i in 1:length(id_zone_B_up_4)){
      data_buff <- table[id_zone_B_up_4[i]:(id_zone_B_up_4[i] + 4), ]
      data_zone_B_up_4 <- do.call(rbind, list(data_zone_B_up_4, data_buff))
    }
  } else if (id_zone_B_up_4 != -1) {
    data_zone_B_up_4 <- table[id_zone_B_up_4:(id_zone_B_up_4 + 4), ]
  }
  
  id_zone_B_low <- v_Position("1111", s_zone_B_low)
  if (length(id_zone_B_low) != 1) {
    for (i in 1:length(id_zone_B_low)){
      data_buff <- table[id_zone_B_low[i]:(id_zone_B_low[i] + 4), ]
      data_zone_B_low <- do.call(rbind, list(data_zone_B_low, data_buff))
    }
  } else if (id_zone_B_low != -1) {
    data_zone_B_low <- table[id_zone_B_low:(id_zone_B_low + 4), ]
  }
  
  id_zone_B_low_2 <- v_Position("11101", s_zone_B_low)
  if (length(id_zone_B_low_2) != 1) {
    for (i in 1:length(id_zone_B_low_2)){
      data_buff <- table[id_zone_B_low_2[i]:(id_zone_B_low_2[i] + 4), ]
      data_zone_B_low_2 <- do.call(rbind, list(data_zone_B_low_2, data_buff))
    }
  } else if (id_zone_B_low_2 != -1) {
    data_zone_B_low_2 <- table[id_zone_B_low_2:(id_zone_B_low_2 + 4), ]
  }
  
  id_zone_B_low_3 <- v_Position("11011", s_zone_B_low)
  if (length(id_zone_B_low_3) != 1) {
    for (i in 1:length(id_zone_B_low_3)){
      data_buff <- table[id_zone_B_low_3[i]:(id_zone_B_low_3[i] + 4), ]
      data_zone_B_low_3 <- do.call(rbind, list(data_zone_B_low_3, data_buff))
    }
    data_zone_B_low_3 <- table[id_zone_B_low_3:(id_zone_B_low_3 + 4), ]
  }
  
  id_zone_B_low_4 <- v_Position("10111", s_zone_B_low)
  if (length(id_zone_B_low_4) != 1) {
    for (i in 1:length(id_zone_B_low_4)){
      data_buff <- table[id_zone_B_low_4[i]:(id_zone_B_low_4[i] + 4), ]
      data_zone_B_low_4 <- do.call(rbind, list(data_zone_B_low_4, data_buff))
    }
  } else if (id_zone_B_low_4 != -1) {
    data_zone_B_low_4 <- table[id_zone_B_low_4:(id_zone_B_low_4 + 4), ]
  }
  
  Fin <-
    do.call(
      rbind,
      list(
        data_zone_B_up,
        data_zone_B_up_2,
        data_zone_B_up_3,
        data_zone_B_up_4,
        data_zone_B_low,
        data_zone_B_low_2,
        data_zone_B_low_3,
        data_zone_B_low_4
      )
    )
  return(unique(Fin))
}

Shewchart_7 <- function(table, sample, average, sigma) {
  data_zone_C <- NULL
  data_buff <- NULL
  lower_c <- average - sigma
  upper_c <- average + sigma
  
  s_zone_C <- s_Sequence_in(sample, lower_c, upper_c)
  id_zone_C <- v_Position("111111111111111", s_zone_C)
  
  if (length(id_zone_C) != 1) {
    for (i in 1:length(id_zone_C)){
      data_buff <- table[id_zone_C[i]:(id_zone_C[i] + 14), ]
      data_zone_C <- do.call(rbind, list(data_zone_C, data_buff))
    }
  } else if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 14), ]
  }
  Fin <- data_zone_C
  return(unique(Fin))
}

Shewchart_8 <- function(table, sample, average, sigma) {
  data_buff <- NULL
  data_zone_C <- NULL
  s_zone_C <- s_Sequence_in(sample, average - sigma, average + sigma)
  id_zone_C <- v_Position("00000000", s_zone_C)
  
  if (length(id_zone_C) != 1) {
    for (i in 1:length(id_zone_C)){
      data_buff <- table[id_zone_C[i]:(id_zone_C[i] + 7), ]
      data_zone_C <- do.call(rbind, list(data_zone_C, data_buff))
    }
  } else  if (id_zone_C != -1) {
    data_zone_C <- table[id_zone_C:(id_zone_C + 7), ]
  }
  Fin <- data_zone_C
  return(unique(Fin))
}

add_trash <- function(plot, table, label) {
  if (!is.null(table)){
  pl <- add_markers(
    p = plot,
    x = table$point,
    y = table$value,
    name = label,
    type = 'scatter',
    mode = 'markers',
    color = I('red')
  )
  return(pl)} else{
    return(plot)
    }
}

view_trash <- function(data, output){
  if (!is.null(data)){
    output$data <- renderTable(
      data
    )
  }
}

get_column_name <-  function(names, choice, datapath, type = "guess") {
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
                        shewh = shew_rl,
                        plot_title = plt_title,
                        x_label = x_lbl,
                        y_label = y_lbl,
                        shew_1 = trash_1,
                        shew_2 = trash_2,
                        shew_3 = trash_3,
                        shew_4 = trash_4,
                        shew_5 = trash_5,
                        shew_6 = trash_6,
                        shew_7 = trash_7,
                        shew_8 = trash_8
                        ) {
  p <- plot_ly(
    name = seq_name,
    data = table,
    type = 'scatter',
    mode = plot_mode,
    # text = text,
    # hoverinfo  = 'text+name',
    x = ~ point,
    y = ~ value
  )
  if (shewh == TRUE){
    print("Rule-loop:")
    print(shew_1)
    p <- add_trash(p, shew_1, "Test №1")
    p <- add_trash(p, shew_2, "Test №2")
    p <- add_trash(p, shew_3, "Test №3")
    p <- add_trash(p, shew_4, "Test №4")
    p <- add_trash(p, shew_5, "Test №5")
    p <- add_trash(p, shew_6, "Test №6")
    p <- add_trash(p, shew_7, "Test №7")
    p <- add_trash(p, shew_8, "Test №8")
    
    print("Rule-loop end")
  }
  
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


trash_1 <<- NULL
trash_2 <<- NULL
trash_3 <<- NULL
trash_4 <<- NULL
trash_5 <<- NULL
trash_6 <<- NULL
trash_7 <<- NULL
trash_8 <<- NULL
shew_rl <<- FALSE

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
    output$Titl <- renderText("Press button <Make QCC> to show data")
    insertUI("#show_vars", "afterEnd",
             tagList(actionButton("form_table", "Confirm"),
                     hr()))
    
  })
  
  observeEvent(input$form_table, {
    if (is.null(read_seq)) {
      read_seq <<- get_column_name(col_names(), col_choice(), input$file1$datapath)
      num_seq <<- read_seq[[1]]
      data_seq <<- tibble(point = 1:length(na.omit(num_seq)), value = na.omit(num_seq))
    } else{
      xaxis_val <<- get_column_name(col_names(), col_choice(), input$file1$datapath)
      s_seq <<- xaxis_val[[1]]
      data_seq <<- tibble(point = s_seq, value = na.omit(num_seq))
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
    Sigm_plt <<- sd(num_seq, na.rm = TRUE)
    Aver_plt <<- mean(num_seq, na.rm = TRUE)
    trash_1 <<- Shewchart_1(data_seq, data_seq$value, Aver_plt + 3*Sigm_plt,  Aver_plt -3*Sigm_plt)
    trash_2 <<- Shewchart_2(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    trash_3 <<- Shewchart_3(data_seq, data_seq$value)
    trash_4 <<- Shewchart_4(data_seq, data_seq$value)
    trash_5 <<- Shewchart_5(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    trash_6 <<- Shewchart_6(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    trash_7 <<- Shewchart_7(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    trash_8 <<- Shewchart_8(data_seq, data_seq$value, Aver_plt, Sigm_plt)
    shew_rl <<- TRUE
    output$plotly <- renderPlotly({
    create_plot(table = data_seq, text = point_info)
    })
    
    output$Titl <- renderText("Analisis Results:")
    if (!is.null(trash_1)){
      output$tr_1 <- renderText("Test №1:")
      trash_1$point <-  as.character(trash_1$point)
      output$trash_1 <- renderTable(expr = trash_1, colnames = FALSE)
    } else {
      output$tr_1 <- renderText("Test №1 - Passed")
    }
    
    if (!is.null(trash_2)){
      output$tr_2 <- renderText("Test №2:")
      trash_2$point <-  as.character(trash_2$point)
      output$trash_2 <- renderTable(expr = trash_2, colnames = FALSE)
    } else {
      output$tr_2 <- renderText("Test №2 - Passed")
    }
    
    if (!is.null(trash_3)){
      output$tr_3 <- renderText("Test №3:")
      trash_3$point <-  as.character(trash_3$point)
      output$trash_3 <- renderTable(expr = trash_3, colnames = FALSE)
    } else {
      output$tr_3 <- renderText("Test №3 - Passed")
    }
    
    if (!is.null(trash_4)){
      output$tr_4 <- renderText("Test №4:")
      trash_4$point <-  as.character(trash_4$point)
      output$trash_4 <- renderTable(expr = trash_4, colnames = FALSE)
    } else {
      output$tr_4 <- renderText("Test №4 - Passed")
    }
    
    if (!is.null(trash_5)){
      output$tr_5 <- renderText("Test №5:")
      trash_5$point <-  as.character(trash_5$point)
      output$trash_5 <- renderTable(expr = trash_5, colnames = FALSE)
    } else {
      output$tr_5 <- renderText("Test №5 - Passed")
    }
    
    if (!is.null(trash_6)){
      output$tr_6 <- renderText("Test №6:")
      trash_6$point <-  as.character(trash_6$point)
      output$trash_6 <- renderTable(expr = trash_6, colnames = FALSE)
    } else {
      output$tr_6 <- renderText("Test №6 - Passed")
    }
    
    if (!is.null(trash_7)){
      output$tr_7 <- renderText("Test №7:")
      trash_7$point <-  as.character(trash_7$point)
      output$trash_7 <- renderTable(expr = trash_7, colnames = FALSE)
    } else {
      output$tr_7 <- renderText("Test №7 - Passed")
    }
    
    if (!is.null(trash_8)){
      output$tr_8 <- renderText("Test №8:")
      trash_8$point <-  as.character(trash_8$point)
      output$trash_8 <- renderTable(expr = trash_8, colnames = FALSE)
    } else {
      output$tr_8 <- renderText("Test №8 - Passed")
    }
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
  
  observeEvent(input$help, {
    showModal(modalDialog(
      title = "Shewhart Tests",
      renderTable(tibble("Test №" = c(1:8), Rule = c(
        "1 point is outside the control limits.",
        "8/9 points on the same size of the center line.",
        "6 consecutive points are steadily increasing or decreasing.",
        "14 consecutive points are alternating up and down.",
        "2 out of 3 consecutive points are more than 2 sigmas from the center line in the same direction.",
        "4 out of 5 consecutive points are more than 1 sigma from the center line in the same direction.",
        "15 consecutive points are within 1 sigma of the center line.",
        "8 consecutive points on either side of the center line with not within 1 sigma."),
        "Problem indicated" = c(
          "A large shift.",
          "A small sustained shift",
          "A trend or drift up or down.",
          "Non-random systematic variation.",
          "A medium shift.",
          "A small shift.",
          "Stratification.",
          "A mixture pattern."))),
      easyClose = TRUE
    ))
  })
  #--- Конец сервера
}
