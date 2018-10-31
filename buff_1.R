# library(plotly)
# library(tidyr)
# 
# df0 <- tibble(x = c("11","22","33","44"),
#               y = c(1, 2, 3, 1))
# df0
# 
# df1 <- tibble(x = c("11","44"),
#               y = c(1, 2))
# df1
# 
# p <- plot_ly(data = df0,
#              x = ~x,
#              y = ~y,
#              name = 'DF0',
#              type = 'scatter',
#              mode = 'markers+lines'
#              )
# 
# p <- add_markers(p = p,
#                  data = df1,
#                  x = ~x,
#                  y = ~y,
#                  name = 'DF0',
#                  type = 'scatter',
#                  mode = 'markers',
#                  color = I('red')
#                  )
#   
# p



library(tidyr) 
library(openxlsx)

library(plotly)

file_path <- 'FRT Weekly Monitoting.xlsx'
Data <- Data_read <- read.xlsx(file_path ,sheet = 1, detectDates = TRUE)

USG_fifth <- Data[Data$Material=="USG - 5k",]
USG_first <- Data[Data$Material=="USG - 1k",]

A_Th_first <- buf <- data.frame(USG_first$Date, USG_first$`Average.Thickness(nm)`)
A_Th_first <- na.omit(A_Th_first)
rownames(A_Th_first)= c(1:length(A_Th_first$USG_first.Date))
names(A_Th_first) <- c('Date','Average_Thickness_nm')
A_Th_first$Average_Thickness_nm <- round(A_Th_first$Average_Thickness_nm,2)
A_Th_first$Date <- as.Date(as.character(A_Th_first$Date))
A_Th_first<-A_Th_first[order(A_Th_first$Date),]

A_Th_fifth <- data.frame(USG_fifth$Date, USG_fifth$`Average.Thickness(nm)`)
A_Th_fifth <- na.omit(A_Th_fifth)
rownames(A_Th_fifth)= c(1:length(A_Th_fifth$USG_fifth.Date))
names(A_Th_fifth) <- c('Date','Average_Thickness_nm')
A_Th_fifth$Average_Thickness_nm <- round(A_Th_fifth$Average_Thickness_nm,2)
#A_Th_fifth$Date <-  as.Date(as.character(A_Th_fifth$Date))
A_Th_fifth<-A_Th_fifth[order(A_Th_fifth$Date),]

Av_usg <- mean(A_Th_first$Average_Thickness_nm, na.rm = TRUE)

Sigm <- sd(A_Th_first$Average_Thickness_nm, na.rm = TRUE)

LCL <- Av_usg - 3 * Sigm
UCL <- Av_usg + 3 * Sigm

values_first <- na.omit(data.frame(Date = USG_first$Date,
                                   Thickness_nm = as.numeric(USG_first$`Thickness(nm)`),
                                   id = rep(1:length(unique(USG_first$Date)), each = 49)))

bands_colors <- c('#FCBBA1', '#FFFFBF', '#C7E9C0',
                  '#C7E9C0', '#FFFFBF', '#FCBBA1')
hov_text <- paste("Date: ", A_Th_first$Date, "<br>", "Value: ",
                  A_Th_first$Average_Thickness_nm, "nm")
val_text <- paste("Date: ", values_first$Date, "<br>", "Value: ",
                  values_first$Thickness_nm, "nm")


p <- plot_ly(data = values_first,
             x = ~Date,
             y = ~Thickness_nm,
             name = 'Thick',
             type = 'scatter',
             mode = 'markers'
             
            )

p <- add_trace(
  p = p,
  x = A_Th_first$Date[1:3],
  y = A_Th_first$Average_Thickness_nm[1:3],
  name = "АААДИН",
  type = 'scatter',
  mode = 'lines+markers',
  color = I('red')
 
)
p

