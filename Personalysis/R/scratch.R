library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))

fig

scatterplot3js(the_matrix,
               # bg = "#000000", grid = TRUE,  pch = "o", stroke = "gray", size=1,
               bg = "#FFFFFF", grid = TRUE,  pch = "o", stroke = "gray", size=1,
               xlim=c(0,6), ylim=c(0,6), zlim=c(0,6), flip.y = FALSE,
               color=c( the_colors)) %>%
  #  points3d(vertices(.), color="white", pch = the_label, size=.20)  %>%
  points3d(vertices(.), color="black", pch = the_label, size=.20)  %>%
  # lines3d(c(lines3d_ndx1),c(lines3d_ndx2), col = "gray", lwd = 1)
  lines3d(c(lines3d_ndx1),c(lines3d_ndx2), col = "black", lwd = 3)


df2     <- subset(personalysis_3d, select = c(r1, r2, r3))
df2 <- personalysis_3d
df2 <- df2 %>% arrange(desc(Executive))
df2$cluster <- as.factor(kmeans(subset(personalysis_3d, select = c(r1, r2, r3)), centers=3, nstart = 25)$cluster )
df2$t1<-ifelse(df2$Executive==1,df2$r1, NA)
df2$t2<-ifelse(df2$Executive==1,df2$r2, NA)
df2$t3<-ifelse(df2$Executive==1,df2$r3, NA)


fig <- plot_ly(df2 , x = ~r1, y = ~r2, z = ~r3, color = ~cluster, colors = c('#0CF4F0','#AE7FFF','#FF9F7F'),text='Name_abbr')
fig <- fig %>% add_markers()
fig <- fig %>% layout(title= 'The Title',
        scene = list(xaxis = list(title = 'Like (Contribute)'),
                     yaxis = list(title = 'Should (Connect)'),
                     zaxis = list(title = 'Must (Commit)',
                                  gridcolor = 'rgb(255, 255, 255)')),
        plot_bgcolor  = 'rgb(243, 243, 243)',
        paper_bgcolor = 'rgb(000, 000, 000)')

fig <- fig %>%  add_lines(x = ~t1, y = ~t2, z = ~t3)
fig


library(bsselectR)
#/Users/tfrench/Desktop/R projects/Personalysis/images/personalysis_',images$file[i],'){width=80%}'
images       <- data.frame(file=c(list.files("/Users/tfrench/Desktop/R projects/Personalysis/images/")))
images       <- dplyr::filter(images, grepl("personalysis_", file))
plots           <- paste0("../images/",images[,1])
names(plots) <- substring(images$file, 14, nchar(images$file)-4)


#bsselect(plots, type = "text", size="10", header=TRUE)

bsselect(plots, type = "img", selected = "Amir",
         live_search = TRUE, show_tick = TRUE)


quotes <- c(unique(Personalysis$Name))

names(quotes) <- c(unique(Personalysis$Name_abbr))

#bsselect(quotes, type = "text", size="10", header=TRUE)
bsselect(my_plots, type = "img", selected = "Oregon",
         live_search = TRUE, show_tick = TRUE)



a<- Personalysis %>% select(Name, n, Dimension, Desc, Desc_long) %>% filter(Desc !='NA') %>%
    tidyr::pivot_wider(names_from = n, values_from = c(Dimension, Desc, Desc_long))

b<- Personalysis %>% filter(n==1) %>% select(-n, -Desc)
c <-left_join(b,a)
