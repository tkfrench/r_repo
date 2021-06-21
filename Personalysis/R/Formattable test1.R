test2 <-bind_rows(test %>% select(Name, Contribution=b1, Connection =b2, Commitment=b3) %>% mutate(Color='Blue (Explore)'),
                  test %>% select(Name, Contribution=g1, Connection =g2, Commitment=g3) %>% mutate(Color='Green) Organize)'),
                  test %>% select(Name, Contribution=r1, Connection =r2, Commitment=r3) %>% mutate(Color='Red (Expadite)'),
                  test %>% select(Name, Contribution=y1, Connection =y2, Commitment=y3) %>% mutate(Color='Yellow ()'))

test3 <-bind_rows(test %>% select(Name, Explore=b1, Organize =g1, Expadite=r1, Collaborate=y1) %>% mutate(Level='Contribution'),
                  test %>% select(Name, Explore=b2, Organize =g2, Expadite=r2, Collaborate=y2) %>% mutate(Level='Connection'),
                  test %>% select(Name, Explore=b3, Organize =g3, Expadite=r3, Collaborate=y3) %>% mutate(Level='Commitment'))
test3 <- test3 %>% select(Name, Level, Explore, Organize, Expadite, Collaborate) %>%
        arrange(Name, Level)

library(formattable)

tom <- test3 %>% filter(Name=='Thomas French')

formattable(test3, align = c('l','l','r','r','r','r'),
            list(
  #age = color_tile("white", "orange"),
  Explore   = color_tile("white", "#195cf7"),
  Organize = color_tile("white", "light green"),
  Expadite   = color_tile("white", "red"),
  Collaborate   = color_tile("white", "GoldenRod")))
