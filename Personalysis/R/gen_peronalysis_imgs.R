#library(htmlwidgets)
#library(collapsibleTree)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
Personalysis <- read_excel("C:/Users/tfrench/Desktop/R projects/r_repo/Personalysis/Data/Personalysis.xlsx")
Strength_desc<- read_excel("C:/Users/tfrench/Desktop/R projects/r_repo/Personalysis/Data/Strengths Abreviated.xlsx")
Personalysis <- merge(Personalysis, Strength_desc, all.x=TRUE)

unlist(gregexpr(" ",Personalysis$Name)) # find 1st space in Name
Personalysis$Name_abbr <- substr(Personalysis$Name, 1, unlist(gregexpr(" ",Personalysis$Name)) + 1)

# Four Quadrant Plot function
four_quadrant <- function(x, col_quad="gray65", col_text="black", title="") {
  nx <- length(x)
  sqx <- sqrt(x)
  df <- data.frame(x=c(sqx[1],-sqx[2],-sqx[3],sqx[4])/2,
                   y=c(sqx[1],sqx[2],-sqx[3],-sqx[4])/2,
                   size=sqx, label=x)
  mm=sqrt(6)
  ggplot(data=df, aes(x=x, y=y, width=size, height=size,
                      group=factor(size))) +
    ggtitle(title) +
    geom_tile(fill=c("#FFD700",'#f08d9f',"#12a608","#86a8f7")) +
    geom_text(aes(label=label), col=col_text, size=6) +
    geom_hline(aes(yintercept=0), size=0.8) +
    geom_vline(aes(xintercept=0), size=0.8) +
    coord_fixed() +
    xlim(c(-mm,mm)) + ylim(c(-mm,mm)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size=20)) +
    theme(legend.position = "none") +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
}

for (i in unique(Personalysis$Name_abbr)) {
    resp <- Personalysis  %>%  filter(!is.na(r1)) %>%  filter(Name_abbr==i)
    footnote = resp$Name[1]
    p1 <- four_quadrant(x=c(resp$y1, resp$r1, resp$g1, resp$b1), title="Contribution") + labs(caption= ' ') + theme(plot.caption = element_text(size=15))
    p2 <- four_quadrant(x=c(resp$y2, resp$r2, resp$g2, resp$b2), title="Connection")   + labs(caption= ' ' ) + theme(plot.caption = element_text(size=15))
    p3 <- four_quadrant(x=c(resp$y3, resp$r3, resp$g3, resp$b3), title="Commitment")   + labs(caption= footnote) + theme(plot.caption = element_text(size=15))
    p4 <- gridExtra::grid.arrange(p1, p2, p3, nrow=1)
    # Save to disk
   ggsave(paste0("Personalysis/images/personalysis_", resp$Name_abbr, ".png"), p4, device="png", width = 30, height = 10, units = "cm",dpi = 72)

}
