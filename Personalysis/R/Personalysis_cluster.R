# Cluster Personalysis scores
#===========================================
# subset to single record per responsent with personalysis coded results
test  <- Personalysis %>% filter(n==1)
lvl   <- list('Cont ', 'Conn ', 'Comm ')

test$all_commit_cluster <- as.character(kmeans(subset(test,select=c(r3,b3,g3,y3)), centers=4, nstart = 25)$cluster)
test$all_commit_Label   <- test$Name

for (clr in c('b','g','r','y')){
  # Kmeans Clustering for each color [r.Expedite, g.Organize, b.Explore, y.Collaborate]
  eval(parse(text = paste0(' test$',clr,'_cluster <- kmeans(subset(test,select=c(',clr,'1,',clr,'2,',clr,'3)), centers=4, nstart = 25)$cluster')))
  # Get mean values for use in labels for COLLAPSED
  # eval(parse(text = paste0(' test$',clr,'_mean <- round(ave(test$',clr,'1,test$',clr,'2,test$',clr,'3),1)')))
  eval(parse(text = paste0(' test$',clr,'_mean <- round( (test$',clr,'1 + test$',clr,'2 + test$',clr,'3)/3,1)')))
  eval(parse(text = paste0(' test$',clr,'_cluster <- paste0("COLLAPSED "',',test$',clr,'_cluster)')))
  eval(parse(text = paste0(' test$',clr,'_Label <- paste0(test$Name," ",test$',clr,'_mean, " ")')))

  for (nbr in c('1','2','3')){
    # Kmeans Clustering for each color by each of 3 sub-levels [1.Contribution, 2.Connection, 3.Communication ]
    eval(parse(text = paste0(' centers <- kmeans(subset(test,select=c(',clr,nbr,')), centers=4, nstart = 25)$centers'))) # First get centers
    eval(parse(text = paste0(' centers <- sort(centers)'))) # Use sorted centers below to manage (order) clustering output
    eval(parse(text = paste0(' test$',clr,nbr,'_cluster <- kmeans(subset(test,select=c(',clr,nbr,')), centers=centers, nstart = 25)$cluster')))
    eval(parse(text = paste0(' test$',clr,nbr,'_cluster <- paste0(lvl[',nbr,'],test$',clr,nbr,'_cluster)')))
    eval(parse(text = paste0(' test$',clr,nbr,'_Label  <- paste0(test$Name," ",test$',clr,nbr,')')))
    # Get level specific labels
    eval(parse(text = paste0(' test$',clr,nbr,'_Label  <- paste0(test$Name," ",test$',clr,nbr,')')))

  }
}
blues2   <- c(rep('#195cf7',1), rep('#86a8f7',4),rep('#dde2f0',length(unique(test$Name))))
greens2  <- c(rep('#12a608',1), rep('#8efa87',4),rep('#e2fae1',length(unique(test$Name))))
reds2    <- c(rep('#fc032c',1), rep('#f08d9f',4),rep('#f2e1e4',length(unique(test$Name))))
yellows2 <- c(rep('#DAA520',1), rep('#FFD700',4),rep('#FFFACD',length(unique(test$Name))))
