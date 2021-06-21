# Generate 3D plot w legend and text for a given color (e.g., r or g) and team (e.g., "Executive")

set.seed(123)
#Personalysis <- read_excel("Data/Personalysis.xlsx")
personalysis_3d <- Personalysis %>% filter(!is.na(r1)) %>% arrange(Name)
personalysis_3d$Name_abbr <- substr(personalysis_3d$Name, 1, unlist(lapply(gregexpr(pattern = ' ', personalysis_3d$Name), min)) +1)


# Create columns for each team with values 0 or 1 as membership
Team      <- data.frame(Group=c("Executive", "Adv_analytics", "CaOps_Dat_AdvA",
                                "Viz_Analytic_Consultants","Prod_Owners_Mgrs", "TomF_vs_JessicaS","LizbethM_vs_GinoC"))
Team$Name     <- list(c("Thomas French", "Gino Conconi", "Caleb Stowell", "Firouzeh Manucheri", "Ari Robicsek"),
                      c("Thomas French", "Chuck Laurenson", "Bradley Roberts"),
                     # c("Thomas French","Susan Pollock","Jessica Swann","Vandana Kapoor"),
                      c("Thomas French","Jessica Swann"),
                      c("Ngoc Salvatierra","Chris Kim","Rocky Ramos","Harry Wolberg","Troy Hanninen","Pete Firth","Kuan Ju Liu"),
                      c("Andy Bennett","Jake Vanderkam","Ashley Snoddy","Sarah Nurse","Jodi Brekhus","Susan Hermanto","Laurel Kirby","Said Sarioghalam","Keyana Azari","Jessica Swann","Jacob Lippa"),
                      c("Thomas French", "Jessica Swann"),
                      c("Gino Conconi", "Lizbeth McNeil") )

personalysis_3d <- personalysis_3d %>%
  mutate( Executive      = ifelse(Name %in% unlist(Team$Name[Team$Group=="Executive"]), 1,0),
          Adv_analytics  = ifelse(Name %in% unlist(Team$Name[Team$Group=="Adv_analytics"]), 1,0),
          CaOps_Dat_AdvA = ifelse(Name %in% unlist(Team$Name[Team$Group=="CaOps_Dat_AdvA"]), 1,0),
          Viz_Analytic_Consultants= ifelse(Name %in% unlist(Team$Name[Team$Group=="Viz_Analytic_Consultants"]), 1,0),
          Prod_Owners_Mgrs= ifelse(Name %in% unlist(Team$Name[Team$Group=="Prod_Owners_Mgrs"]), 1,0),
          TomF_vs_JessicaS= ifelse(Name %in% unlist(Team$Name[Team$Group=="TomF_vs_JessicaS"]), 1,0),
          LizbethM_vs_GinoC= ifelse(Name %in% unlist(Team$Name[Team$Group=="LizbethM_vs_GinoC"]), 1,0))

Executive_label     <- ifelse(personalysis_3d$Executive== 1,personalysis_3d$Name_abbr,'')
Adv_analytics_label <- ifelse(personalysis_3d$Adv_analytics== 1,personalysis_3d$Name_abbr,'')
CaOps_Dat_AdvA_label<- ifelse(personalysis_3d$CaOps_Dat_AdvA== 1,personalysis_3d$Name_abbr,'')
Viz_Analytic_Consultants_label<- ifelse(personalysis_3d$Viz_Analytic_Consultants== 1,personalysis_3d$Name_abbr,'')
Prod_Owners_Mgrs_label<- ifelse(personalysis_3d$Prod_Owners_Mgrs== 1,personalysis_3d$Name_abbr,'')
TomF_vs_JessicaS_label<- ifelse(personalysis_3d$TomF_vs_JessicaS== 1,personalysis_3d$Name_abbr,'')
LizbethM_vs_GinoC_label<- ifelse(personalysis_3d$LizbethM_vs_GinoC== 1,personalysis_3d$Name_abbr,'')

Executive_ndx      <- which(personalysis_3d$Executive      == 1)
Adv_analytics_ndx  <- which(personalysis_3d$Adv_analytics  == 1)
CaOps_Dat_AdvA_ndx <- which(personalysis_3d$CaOps_Dat_AdvA == 1)
Viz_Analytic_Consultants_ndx <- which(personalysis_3d$Viz_Analytic_Consultants== 1)
Prod_Owners_Mgrs_ndx  <- which(personalysis_3d$Prod_Owners_Mgrs== 1)
TomF_vs_JessicaS_ndx  <- which(personalysis_3d$TomF_vs_JessicaS== 1)
LizbethM_vs_GinoC_ndx <- which(personalysis_3d$LizbethM_vs_GinoC== 1)

Executive_ndx2     <- lag(Executive_ndx); Executive_ndx2[1]= Executive_ndx[length(Executive_ndx)]
Adv_analytics_ndx2 <- lag(Adv_analytics_ndx); Adv_analytics_ndx2[1]= Adv_analytics_ndx[length(Adv_analytics_ndx)]
CaOps_Dat_AdvA_ndx2<- lag(CaOps_Dat_AdvA_ndx); CaOps_Dat_AdvA_ndx2[1]= CaOps_Dat_AdvA_ndx[length(CaOps_Dat_AdvA_ndx)]
Viz_Analytic_Consultants_ndx2<- lag(Viz_Analytic_Consultants_ndx); Viz_Analytic_Consultants_ndx2[1]= Viz_Analytic_Consultants_ndx[length(Viz_Analytic_Consultants_ndx)]
Prod_Owners_Mgrs_ndx2<- lag(Prod_Owners_Mgrs_ndx); Prod_Owners_Mgrs_ndx2[1]= Prod_Owners_Mgrs_ndx[length(Prod_Owners_Mgrs_ndx)]
TomF_vs_JessicaS_ndx2<- lag(TomF_vs_JessicaS_ndx); TomF_vs_JessicaS_ndx2[1]= TomF_vs_JessicaS_ndx[length(TomF_vs_JessicaS_ndx)]
LizbethM_vs_GinoC_ndx2<- lag(LizbethM_vs_GinoC_ndx); LizbethM_vs_GinoC_ndx2[1]= LizbethM_vs_GinoC_ndx[length(LizbethM_vs_GinoC_ndx)]


the_image_path = 'Users/tfrench/Desktop/R projects/Personalysis/images'


if( the_color=='r') {the_matrix <- as.matrix(subset(personalysis_3d, select = c(r1, r2, r3)))}  # Subset data by specified the_color, as matrix
if( the_color=='b') {the_matrix <- as.matrix(subset(personalysis_3d, select = c(b1, b2, b3)))}
if( the_color=='g') {the_matrix <- as.matrix(subset(personalysis_3d, select = c(g1, g2, g3)))}
if( the_color=='y') {the_matrix <- as.matrix(subset(personalysis_3d, select = c(y1, y2, y3)))}
colnames(the_matrix) <- c("Like (Contribute)", " Should (Connect)", " Must (Commit)")        # 3D plot vertex labels

the_colors <- kmeans(the_matrix, centers=3, nstart = 25)$cluster  # Assign colors to each point based on clustered value (3)
the_colors <- ifelse(the_colors==1,'#0CF4F0',ifelse(the_colors==2,'#AE7FFF', ifelse(the_colors==3,'#FF9F7F','yellow')))


the_lgnd_img<-ifelse(the_color=='r', 'r1r2r3b0b0b0g0g0g0y0y0y0.png',   # Legend image for specified color
              ifelse(the_color=='b', 'r0r0r0b1b2b3g0g0g0y0y0y0.png',
              ifelse(the_color =='g', 'r0r0r0b0b0b0g1g2g3y0y0y0.png',
                                     'r0r0r0b0b0b0g0g0g0y1y2y3.png')))
the_html <- paste0('\n')
the_html <- paste0(the_html, '<table><tr><td  width = "235">  ![](/',the_image_path,'/', the_lgnd_img, ')</td>'  )
the_html2 <- ifelse(the_color=='r', paste0('<td><B> <span style="color:darkred;">',
                                          'Expedite - Red </span>is motivated to</B>:<BR>Initiate action <BR> Stay moving <BR> Focus on tasks and tangible outcomes'),
                   ifelse(the_color=='b', paste0('<td><B> <span style="color:blue;">',
                                                 'Explore - Blue </span>is motivated to</B>:<BR>Anticipate the future <BR> Create clarity <BR> Focus on context, purpose, and potential impacts'),
                          ifelse(the_color=='g', paste0('<td><B> <span style="color:darkgreen;">',
                                                        '	Organize - Green</span> is motivated to</B>:<BR> Maintain stability and organization <BR> Focus on details, structure and predictability'),
                                 ifelse(the_color=='y', paste0('<td><B> <span style="color:goldenrod;">',
                                                               'Collaborate - Yellow</span> is motivated to</B>:<BR> Adapt and be flexible <BR> Focus on people and possibilities')))))
the_html <- paste0(the_html, the_html2, '</td></tr> </table>  \n  \n  '  )

eval(parse(text = paste0('the_label     = ', the_team, '_label')))    # the_label:    3d Plot point labels for team members only, others are blank
eval(parse(text = paste0('lines3d_ndx1  = ', the_team, '_ndx')))      # lines3d_ndx1: 1st of paired coords. for connecting points w line on 3d plot
eval(parse(text = paste0('lines3d_ndx2  = ', the_team, '_ndx2')))     #lines3d_ndx2:  2nd of paired coords. for connecting points w line on 3d plot

cat('\n')
cat(the_html)
cat('  \n')
scatterplot3js(the_matrix,
              # bg = "#000000", grid = TRUE,  pch = "o", stroke = "gray", size=1,
               bg = "#FFFFFF", grid = TRUE,  pch = "o", stroke = "gray", size=1,
               xlim=c(0,6), ylim=c(0,6), zlim=c(0,6), flip.y = FALSE,
               color=c( the_colors)) %>%
 #  points3d(vertices(.), color="white", pch = the_label, size=.20)  %>%
  points3d(vertices(.), color="black", pch = the_label, size=.20)  %>%
 # lines3d(c(lines3d_ndx1),c(lines3d_ndx2), col = "gray", lwd = 1)
  lines3d(c(lines3d_ndx1),c(lines3d_ndx2), col = "black", lwd = 3)

cat('  \n')
cat('Clusters: ![](/Users/tfrench/Desktop/R projects/Personalysis/images/color_map_wht.png)')

