library(threejs)
library(collapsibleTree)
library(htmlwidgets)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

set.seed(123)

Personalysis <- read_excel("C:/Users/tfrench/Desktop/R projects/Personalysis/Data/Personalysis.xlsx")
Strength_desc<- read_excel("C:/Users/tfrench/Desktop/R projects/Personalysis/Data/Strengths Abreviated.xlsx")
Personalysis <- merge(Personalysis, Strength_desc, all.x=TRUE)
Personalysis$Name_abbr <- substr(Personalysis$Name, 1, unlist(gregexpr(" ",Personalysis$Name)) + 1)
respondents   <- unique(Personalysis$Name)
images       <- data.frame(file=c(list.files("/Users/tfrench/Desktop/R projects/Personalysis/images/")))
#images       <- dplyr::filter(images, grepl("personalysis01_", file))
images       <- dplyr::filter(images, grepl("personalysis_", file))
images$file  <- substring(images$file, 14)
images$name  <- substring(images$file, 1, nchar(images$file)-4)

a1<- Personalysis %>% select(Name, Name_abbr, n, Dimension, Desc, Desc_long) %>%
                      filter(Desc !='NA') %>%
                      tidyr::pivot_wider(names_from = n, values_from = c(Dimension, Desc, Desc_long)) %>%
                      mutate(
             Dim.Executing =    ifelse(Dimension_1=='Executing',1,0) + ifelse(Dimension_2=='Executing',1,0) *
                                ifelse(Dimension_3=='Executing',1,0) + ifelse(Dimension_4=='Executing',1,0)+
                                ifelse(Dimension_5=='Executing',1,0),

             Dim.Influencing =  ifelse(Dimension_1=='Influencing',1,0) + ifelse(Dimension_2=='Influencing',1,0) *
                                ifelse(Dimension_3=='Influencing',1,0) + ifelse(Dimension_4=='Influencing',1,0)+
                                ifelse(Dimension_5=='Influencing',1,0),

             Dim.Strategic_Thinking =  ifelse(Dimension_1=='Strategic Thinking',1,0) + ifelse(Dimension_2=='Strategic Thinking',1,0) *
                                       ifelse(Dimension_3=='Strategic Thinking',1,0) + ifelse(Dimension_4=='Strategic Thinking',1,0)+
                                       ifelse(Dimension_5=='Strategic Thinking',1,0),

             Dim.Relationship_Building =  ifelse(Dimension_1=='Relationship Building',1,0) + ifelse(Dimension_2=='Relationship Building',1,0) *
                                          ifelse(Dimension_3=='Relationship Building',1,0) + ifelse(Dimension_4=='Relationship Building',1,0)+
                                          ifelse(Dimension_5=='Relationship Building',1,0)
             )

a2<- Personalysis %>% filter(r1!='NA') %>% select(Name, Name_abbr, r1,r2,r3,b1,b2,b3,g1,g2,g3,y1,y2,y3)

a3 <- merge(a1,a2)
a3 <- a3 %>% select(Name_abbr, r1,r2,r3,b1,b2,b3,g1,g2,g3,y1,y2,y3, Dim.Executing, Dim.Influencing, Dim.Strategic_Thinking, Dim.Relationship_Building )

a3<- rename(a3, c(Expedite_Contribute_R1=r1, Expedite_Connect_R2=r2 , Expedite_Commit_R3=r3 ))
a3<- rename(a3, c(Explore_Contribute_B1=b1, Explore_Connect_B2=b2 , Explore_Commit_B3=b3 ))
a3<- rename(a3, c(Organize_Contribute_G1=g1, Organize_Connect_G2=g2 , Organize_Commit_G3=g3 ))
a3<- rename(a3, c(Collaborate_Contribute_Y1=y1, Collaborate_Connect_Y2=y2 , Collaborate_Commit_Y3=y3 ))
a3<- rename(a3, c(Relationship_Building_Strength = Dim.Relationship_Building, Strategic_Thinking_Strength =Dim.Strategic_Thinking,
                  Influencing_Strength =Dim.Influencing, Executing_Strength = Dim.Executing))

cormat <- round(cor(a3[,2:17], method="spearman", use="complete.obs"),2)


library(reshape2)
melted_cormat <- melt(cormat)


get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Reorder the correlation matrix
#cormat        <- reorder_cormat(cormat)
upper_tri     <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap     <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Spearman\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1))+
  coord_fixed()

ggheatmap +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3.5) +
  ggtitle("Correlations: Personalysis and Strengths") +
  theme(
    text = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  labs(caption = "|--------------------------- Personalysis Traits -----------------------------| |------ Strengths ------|")


# PCA
######################################
## http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
PCA_results <- prcomp(a3[,2:17], scale = TRUE)

#create scree plot

library("factoextra")
eig.val <- get_eigenvalue(PCA_results)
eig.val
fviz_eig(PCA_results, addlabels = TRUE, ylim = c(0, 50), main="PCA: Personalysis Components and Strengths Scree Plot")
#fviz_pca_var(PCA_results, col.var = "black")
#fviz_cos2(PCA_results, choice = "var", axes = 1:2)

var <- get_pca_var(PCA_results)
library("corrplot")
cex.before <- par("cex")
par(cex = 0.8)
corrplot(var$cos2[,1:8], is.corr=FALSE, main="PCA: Personalysis and Strengths Corrs with PCA Dimensions" , mar = c(1, 1, 2, 1))
par(cex = cex.before)
#var <- get_pca_var(prcomp(a2[,3:14]))
#corrplot(var$cos2[,1:8], is.corr=FALSE, main="PCA: Personalysis and Strengths Corrs with PCA Dimensions", mar = c(1, 1, 2, 1))



