fig <- plot_ly(regdata, x= ~x, y = ~y, type = 'bar',
               text = y, texposition = 'auto',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107')))

fig <- fig %>% layout(title = "Regional Job Performance",
                      xaxis = list(title = "Region"),
                      yaxis = list(title = "Job Performance"),
                      width = '200',
                      height = '200')
fig
