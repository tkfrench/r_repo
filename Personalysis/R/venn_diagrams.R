
quad_venn = function(df, color, strength){
  library("VennDiagram")
  if(color=='red')    the_colors = c("black", "red1", "red2", "red3")
  if(color=='blue')   the_colors = c("black", "blue", "blue2", "blue3")
  if(color=='green')  the_colors = c("black", "green1", "green2", "green3")
  if(color=='yellow') the_colors = c("black", "yellow1", "yellow2", "yellow3")

  if(strength=='Executing')             cat = c("Executing\n Strength", "Contribute", "Connect", "Commit")
  if(strength=='Influencing')           cat = c("Influenting\n Strength", "Contribute", "Connect", "Commit")
  if(strength=='Relationship Building') cat = c("Relationship\n\ Building", "Contribute", "Connect", "Commit")
  if(strength=='Strategic Thinking')    cat = c("Strategic\n Thinking\n", "Contribute", "Connect", "Commit")

  if(strength=='Executing')             strgth = df$Executing
  if(strength=='Influencing')           strgth = df$Influencing
  if(strength=='Relationship Building') strgth = df$Relationship_Building
  if(strength=='Strategic Thinking')    strgth = df$Strategic_Thinking
  if(color=='red')   {c1=df$Expedite_Contribute_R1; c2 = df$Expedite_Connect_R2; c3 = df$Expedite_Commit_R3}
  if(color=='blue')  {c1=df$Explore_Contribute_B1;  c2 = df$Explore_Connect_B2;  c3 = df$Explore_Commit_B3}
  if(color=='green') {c1=df$Organize_Contribute_G1;  c2 = df$Organize_Connect_G2;  c3 = df$Organize_Commit_G3}
  if(color=='yellow'){c1=df$Collaborate_Contribute_Y1;  c2 = df$Collaborate_Connect_Y2;  c3 = df$Collaborate_Commit_Y3}

  grid.newpage()
  venn<- draw.quad.venn(
    area1 = sum(ifelse(c1 >= 3.5, 1, 0)) ,
    area2 = sum(ifelse(c2 >= 3.5, 1, 0)) ,
    area3 = sum(ifelse(c3 >= 3.5, 1, 0)) ,
    area4 = sum(strgth),
    n12   = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c2 >= 3.5, 1, 0)),
    n13   = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0)),
    n14   = sum(ifelse(c1 >= 3.5, 1, 0) * sign(strgth)),
    n23   = sum(ifelse(c2 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0)),
    n24   = sum(ifelse(c2 >= 3.5, 1, 0) * sign(strgth)),
    n34   = sum(ifelse(c3 >= 3.5, 1, 0) * sign(strgth)),
    n123  = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c2 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0)),
    n124  = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c2 >= 3.5, 1, 0) * sign(strgth)),
    n134  = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0) * sign(strgth)),
    n234  = sum(ifelse(c2 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0) * sign(strgth)),
    n1234 = sum(ifelse(c1 >= 3.5, 1, 0) * ifelse(c2 >= 3.5, 1, 0) * ifelse(c3 >= 3.5, 1, 0) * sign(strgth)),
    category = cat,
    fill = the_colors,
    lty = "dashed",
    cex = 1,
    cat.cex = 1,
    cat.col =  the_colors)
  return(venn)
  }
test <- quad_venn(df=a3, color='red', strength='Executing')
test <- quad_venn(df=a3, color='blue', strength='Executing')
test <- quad_venn(df=a3, color='green', strength='Executing')
test <- quad_venn(df=a3, color='yellow', strength='Executing')

test <- quad_venn(df=a3, color='red', strength='Influencing')
test <- quad_venn(df=a3, color='blue', strength='Influencing')
test <- quad_venn(df=a3, color='green', strength='Influencing')
test <- quad_venn(df=a3, color='yellow', strength='Influencing')

test <- quad_venn(df=a3, color='red', strength='Relationship Building')
test <- quad_venn(df=a3, color='blue', strength='Relationship Building')
test <- quad_venn(df=a3, color='green', strength='Relationship Building')
test <- quad_venn(df=a3, color='yellow', strength='Relationship Building')

test <- quad_venn(df=a3, color='red', strength='Strategic Thinking')
test <- quad_venn(df=a3, color='blue', strength='Strategic Thinking')
test <- quad_venn(df=a3, color='green', strength='Strategic Thinking')
test <- quad_venn(df=a3, color='yellow', strength='Strategic Thinking')

