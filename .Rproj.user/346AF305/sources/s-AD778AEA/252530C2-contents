create_plot <- function(tabl, text){
  p <- plot_ly(
    name = "Your Sequence",
    data = tabl,
    type = 'scatter',
    mode = 'lines+markers',
    text = text,
    hoverinfo  = 'text+name',
    x = ~point,
    y = ~value
  )
  p <- layout(p=p,
              title = 'Value distribution',
              legend = list(orientation = 'h'),
              xaxis = list(
                type = 'category'
              )
  )
  return(p)
}