#set.seed(123)
Personalysis <- read_excel("Data/Personalysis.xlsx")
personalysis_sample <- Personalysis %>% filter(!is.na(r1))

personalysis_sample <- personalysis_sample %>% arrange(Name)
personalysis_sample$Name_abbr <- substr(personalysis_sample$Name, 1, unlist(lapply(gregexpr(pattern = ' ', personalysis_sample$Name), min)) +1)


r_mdat    <- as.matrix(subset(personalysis_sample, select = c(r1, r2, r3)))
r_colors  <- kmeans(r_mdat, centers=3, nstart = 25)$cluster
#r_colors2 <- ifelse(r_colors==1,'red',ifelse(r_colors==2,'green', ifelse(r_colors==3,'blue','yellow')))
r_colors2 <- ifelse(r_colors==1,'#0CF4F0',ifelse(r_colors==2,'#AE7FFF', ifelse(r_colors==3,'#FF9F7F','yellow')))
colnames(r_mdat) <- c("Like (Contribute)", " Should (Connect)", " Must (Commit)")

g_mdat    <- as.matrix(subset(personalysis_sample, select = c(g1, g2, g3)))
g_colors  <- kmeans(g_mdat, centers=3, nstart = 25)$cluster
#g_colors2 <- ifelse(g_colors==1,'red',ifelse(g_colors==2,'green', ifelse(g_colors==3,'blue','yellow')))
g_colors2 <- ifelse(g_colors==1,'#0CF4F0',ifelse(g_colors==2,'#AE7FFF', ifelse(g_colors==3,'#FF9F7F','yellow')))
colnames(g_mdat) <- c("Like (Contribute)", " Should (Connect)", " Must (Commit)")

b_mdat    <- as.matrix(subset(personalysis_sample, select = c(b1, b2, b3)))
b_colors  <- kmeans(b_mdat, centers=3, nstart = 25)$cluster
#b_colors2 <- ifelse(b_colors==1,'red',ifelse(b_colors==2,'green', ifelse(b_colors==3,'blue','yellow')))
b_colors2 <- ifelse(b_colors==1,'#0CF4F0',ifelse(b_colors==2,'#AE7FFF', ifelse(b_colors==3,'#FF9F7F','yellow')))
colnames(b_mdat) <- c("Like (Contribute)", " Should (Connect)", " Must (Commit)")

y_mdat    <- as.matrix(subset(personalysis_sample, select = c(y1, y2, y3)))
y_colors  <- kmeans(y_mdat, centers=3, nstart = 25)$cluster
y_colors2 <- ifelse(y_colors==1,'#0CF4F0',ifelse(y_colors==2,'#AE7FFF', ifelse(y_colors==3,'#FF9F7F','yellow')))
colnames(y_mdat) <- c("Like (Contribute)", " Should (Connect)", " Must (Commit)")



# Create columns for each team with values 0 or 1 as membership
Team      <- data.frame(Group=c("Executive", "Adv_analytics", "CaOps_Dat_AdvA"))
Team$Name     <- list(c("Thomas French", "Gino Conconi", "Caleb Stowell", "Firouzeh Manucheri", "Ari Robicsek"),
                      c("Thomas French", "Chuck Laurenson", "Bradley Roberts"),
                      c("Thomas French","Susan Pollock","Jessica Swann","Vandana Kapoor"))

personalysis_sample <- personalysis_sample %>%
  mutate( Executive      = ifelse(Name %in% unlist(Team$Name[Team$Group=="Executive"]), 1,0),
          Adv_analytics  = ifelse(Name %in% unlist(Team$Name[Team$Group=="Adv_analytics"]), 1,0),
          CaOps_Dat_AdvA = ifelse(Name %in% unlist(Team$Name[Team$Group=="CaOps_Dat_AdvA"]), 1,0))
Executive_label     <- ifelse(personalysis_sample$Executive== 1,personalysis_sample$Name_abbr,'')
Adv_analytics_label <- ifelse(personalysis_sample$Adv_analytics== 1,personalysis_sample$Name_abbr,'')
CaOps_Dat_AdvA_label<- ifelse(personalysis_sample$CaOps_Dat_AdvA== 1,personalysis_sample$Name_abbr,'')

Executive_ndx      <- which(personalysis_sample$Executive      == 1)
Adv_analytics_ndx  <- which(personalysis_sample$Adv_analytics  == 1)
CaOps_Dat_AdvA_ndx <- which(personalysis_sample$CaOps_Dat_AdvA == 1)

Executive_ndx2     <- lag(Executive_ndx); Executive_ndx2[1]= Executive_ndx[length(Executive_ndx)]
Adv_analytics_ndx2 <- lag(Adv_analytics_ndx); Adv_analytics_ndx2[1]= Adv_analytics_ndx[length(Adv_analytics_ndx)]
CaOps_Dat_AdvA_ndx2<- lag(CaOps_Dat_AdvA_ndx); CaOps_Dat_AdvA_ndx2[1]= CaOps_Dat_AdvA_ndx[length(CaOps_Dat_AdvA_ndx)]
