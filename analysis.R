install.packages("tidyverse")
library("tidyverse")
library(RColorBrewer)
incarceration_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
display.brewer.all()

incarceration_data <- incarceration_data %>% 
  drop_na()


# What is the maximum population of incarcerated Black males?
max_b_male_jailed <- incarceration_data %>% 
  select(black_male_prison_pop) %>% 
  filter(black_male_prison_pop == max(black_male_prison_pop)) %>% 
  pull(black_male_prison_pop)

# What is the maximum population of incarcerated White males?
max_w_male_jailed <- incarceration_data %>% 
  select(white_male_prison_pop) %>% 
  filter(white_male_prison_pop == max(white_male_prison_pop)) %>% 
  pull(white_male_prison_pop)

# What is the maximum population of incarcerated Black males in 2000?
b_male_pop_2000 <- incarceration_data %>% 
  select(black_male_prison_pop, year) %>% 
  filter(year == 2000) %>% 
  filter(black_male_prison_pop == max(black_male_prison_pop)) %>% 
  pull(black_male_prison_pop)

# What is the maximum population of incarcerated White males in 2000?
w_male_pop_2000 <- incarceration_data %>% 
  select(white_male_prison_pop, year) %>% 
  filter(year == 2000) %>% 
  filter(white_male_prison_pop == max(white_male_prison_pop)) %>% 
  pull(white_male_prison_pop)

# What is the maximum population of incarcerated Black males in 2013?
b_male_pop_2013 <- incarceration_data %>% 
  select(black_male_prison_pop, year) %>% 
  filter(year == 2013) %>% 
  filter(black_male_prison_pop == max(black_male_prison_pop)) %>% 
  pull(black_male_prison_pop)

# What is the maximum population of incarcerated White males in 2013?
w_male_pop_2013 <- incarceration_data %>% 
  select(white_male_prison_pop, year) %>% 
  filter(year == 2013) %>% 
  filter(white_male_prison_pop == max(white_male_prison_pop)) %>% 
  pull(white_male_prison_pop)

# What county has the highest number of incarcerated Black males?
b_male_popc <- incarceration_data %>% 
  select(black_male_prison_pop, county_name) %>% 
  filter(black_male_prison_pop == max(black_male_prison_pop)) %>% 
  pull(county_name)

# What county has the highest number of incarcerated White males?
w_male_popc <- incarceration_data %>% 
  select(white_male_prison_pop, county_name) %>% 
  filter(white_male_prison_pop == max(white_male_prison_pop)) %>% 
  pull(county_name)

# What county has the lowest number of incarcerated Black males?
b_male_popcm <- incarceration_data %>% 
  select(black_male_prison_pop, county_name) %>% 
  filter(black_male_prison_pop == min(black_male_prison_pop)) %>% 
  select(county_name, black_male_prison_pop)

# What county has the lowest number of incarcerated White males?
w_male_popcm <- incarceration_data %>% 
  select(white_male_prison_pop, county_name) %>% 
  filter(white_male_prison_pop == min(white_male_prison_pop)) %>% 
  select(county_name, white_male_prison_pop)

# Charts
m_race_incarcerated <- incarceration_data %>% 
  select(year, white_male_prison_pop, black_male_prison_pop) %>% 
  group_by(year) %>% 
  summarise(year = unique(year),white_male_prison_pop = sum(white_male_prison_pop),
            black_male_prison_pop = sum(black_male_prison_pop), .groups = "drop" )
bw_incarceration <- m_race_incarcerated %>% 
  select(year, white_male_prison_pop, black_male_prison_pop) %>% 
  gather(key = race, value = jail_population, -year) %>% 
  group_by(race, jail_population)
one_graph <- ggplot(data = bw_incarceration)+
  geom_line(mapping = aes(x=year, y= jail_population, color=race), size=2)+
  labs(title = "Black and White Incarceration Throughout Time",
       x= "Year",
       y= "Jail Population ",
       color="Race")

for (i in 1:length(bw_incarceration$race)) {
  if(bw_incarceration$race[i]== "black_male_prison_pop"){
    bw_incarceration$race[i]= "Black Male Prison Population"
  }  else if (bw_incarceration$race[i]== "white_male_prison_pop"){
    bw_incarceration$race[i]= "White Male Prison Population"
}
}

bmale_trend <- incarceration_data %>% 
  select(male_jail_pop, year)

b_line_max <- ggplot(data = bmale_trend)+
  geom_col(mapping = aes(x=year, y= male_jail_pop))+
  labs(title = "Male Incarceration Throughout Time",
       x= "Year",
       y= "Male Jail Population")



# Map

install.packages("maps")
library(maps)
install.packages("mapproj")
library(mapproj)

bw_map <- incarceration_data %>% 
  select(fips, state, county_name, year, black_male_prison_pop)

join_bwfips <- map_data("county") %>% 
  unite(polyname, region, subregion, sep = ",")

library(dplyr)
joinb <- left_join(join_bwfips, county.fips)

bwmale_join <- full_join(bw_map, joinb)

make_bw_map <- ggplot(data = bwmale_join)+
  geom_polygon(mapping = aes(x=long,y=lat, fill=black_male_prison_pop),color="grey")+
  scale_fill_continuous(limits=c(0,max(bwmale_join$black_male_prison_pop)), low="yellow",
                        high="red")+
  labs(title = "Black Incarceration Population Map", fill="Black Male Prison Pop")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )




  

  








  









