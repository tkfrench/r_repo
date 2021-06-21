library(threejs)
library(collapsibleTree)
library(htmlwidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)


Personalysis <- read_excel("C:/Users/tfrench/Desktop/R projects/Personalysis/Data/Personalysis.xlsx")
Strength_desc<- read_excel("C:/Users/tfrench/Desktop/R projects/Personalysis/Data/Strengths Abreviated.xlsx")
Personalysis <- merge(Personalysis, Strength_desc, all.x=TRUE)
Personalysis$Name_abbr <- substr(Personalysis$Name, 1, unlist(gregexpr(" ",Personalysis$Name)) + 1)
respondents   <- unique(Personalysis$Name)


s <-Personalysis%>%
  select(Name_abbr, Dimension, Desc) %>%
  group_by(Name_abbr, Dimension, Desc) %>%
  arrange(Name_abbr) %>%
  drop_na() %>%
  mutate(Executing             = ifelse(Dimension=='Executing', Desc, ''),
         Influencing           = ifelse(Dimension=='Influencing', Desc, ''),
         Relationship_Building = ifelse(Dimension=='Relationship Building', Desc, ''),
         Strategic_Thinking    = ifelse(Dimension=='Strategic Thinking', Desc, '')) %>%
  ungroup() %>%
  select(-Dimension, -Desc)


 s1 <- s %>% select(Name_abbr, Executing)   %>% arrange(Name_abbr, desc(Executing)) %>%  mutate(id=row_number())
 s2 <- s %>% select(Name_abbr, Influencing) %>% arrange(Name_abbr, desc(Influencing)) %>%  mutate(id=row_number())
 s3 <- s %>% select(Name_abbr, Relationship_Building) %>% arrange(Name_abbr, desc(Relationship_Building)) %>%  mutate(id=row_number())
 s4 <- s %>% select(Name_abbr, Strategic_Thinking) %>% arrange(Name_abbr, desc(Strategic_Thinking)) %>%  mutate(id=row_number())

 s5 <- merge(merge(merge(s1,s2),s3),s4) %>%
       arrange() %>%
       select(-id) %>%
       filter(!(Executing =='' & Influencing =='' & Relationship_Building=='' & Strategic_Thinking =='' ))
 library(DT)
 s5 %>% filter(Name_abbr=='Thomas F') %>%
        select(Executing, Influencing, Relationship_Building, Strategic_Thinking) %>%
#datatable(data=s5,rownames=FALSE, options = list(dom = 't')) %>%
   datatable(rownames=FALSE, options = list(dom = 't',ordering=F),
          colnames=c("Executing", "Influencing","Relationship Building", "Strategic Thinking")) %>%
          formatStyle('Executing',backgroundColor='#D8BFD8')%>%
          formatStyle('Influencing',backgroundColor='orange')%>%
          formatStyle('Relationship_Building',backgroundColor='#87CEEB')%>%
          formatStyle('Strategic_Thinking',backgroundColor='#98FB98')


 names <- as.vector(Personalysis %>% select(Name_abbr,Dimension,Desc_long) %>% drop_na() %>% select(Name_abbr) )
 ndx = sample(seq(1,nrow(names)),1)
 the_name =names[ndx,]

 the_name

Personalysis %>% select(Name_abbr,Dimension,Desc_long) %>% drop_na() %>%
                 group_by(Dimension, Desc_long) %>%
                 ungroup() %>%
                 arrange(Name_abbr) %>%
                filter(Name_abbr== the_name) %>%
                 select(Dimension, Desc_long) %>%
                 arrange(Dimension, Desc_long) %>%
               datatable(rownames=FALSE, colnames=c(""),

                         class = 'stripe hover compact cell-border row-border',

                         options = list(dom = 't',ordering=F,
                                        initComplete = JS(
                                          "function(settings, json) {",
                                          "$('body').css({'font-family': 'Calibri'});",
                                          "}"
                                        )) ) %>%
  formatStyle(
    'Dimension', fontWeight = 'bold',
    backgroundColor = styleEqual(
      unique(Personalysis$Dimension), c("#7901AF55", "#ED7D3155","#4472C455", "#70AD4755","grey")
    )
  )

the_name




par(mar = c(0,0,0,0))
plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n') +

text(x = 0.000, y = 0.95, paste("EXPEDITE"), cex = 1, col = "red", family="serif", font=1.5, adj=0.0) +

text(x = 0.000, y = 0.70, paste("EXPLORE"), cex = 1, col = "blue", family="serif", font=1.5, adj=0.0) +


text(x = 0.000, y = 0.45, paste("ORGANIZE"), cex = 1, col = "darkgreen", family="serif", font=1.5, adj=0.0) +


text(x = 0.000, y = 0.20, paste("COLLABORATE"), cex = 1, col = "goldenrod", family="serif", font=1.5, adj=0.0)


my_plot <- recordPlot()
plot.new()
my_plot


library(magick)
frink <- image_read("https://jeroen.github.io/images/frink.png")
print(frink)
image_info(frink)

bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
frink <- image_read("https://jeroen.github.io/images/frink.png")
logo <- image_read("https://jeroen.github.io/images/Rlogo.png")
img <- c(bigdata, logo, frink)
img <- image_scale(img, "300x300")
image_info(img)

print(img)
image_append(image_scale(img, "x200"))

newlogo <- image_scale(image_read("https://jeroen.github.io/images/Rlogo.png"))
oldlogo <- image_scale(image_read("https://developer.r-project.org/Logo/Rlogo-3.png"))
image_resize(c(oldlogo, newlogo), '250x150!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate()

my_image  <- image_scale(image_read("C:/Users/tfrench/Desktop/R projects/Personalysis/images/CA_logo_A.png"))
my_image2 <- image_scale(image_read("C:/Users/tfrench/Desktop/R projects/Personalysis/images/CA_logo_B.png"))
my_image3 <- image_resize(c(my_image, my_image2), '347x112!') %>%
  image_background('white') %>%
  image_morph() %>%
  image_animate(fps=5, optimize=T)
#my_image3
my_image3[c(rep(1, times=12),1:10,rep(10, times=12),10:1 )]
