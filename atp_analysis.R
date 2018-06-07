# Nisha Patel
# Exploratory Analysis of 2012-2017 ATP Men's Tennis Data
# Note: Data runs from January 2012-July 2017 only
# Dataset from Kaggle: https://www.kaggle.com/m3financial/atp-tennis-data-from-201201-to-201707

#----------Helper function to install necessary packages----------#
install_packages <- function(pkg) { 
  
  # Install package if it is not already
  if (!(pkg %in% installed.packages()[, "Package"])){ 
    
    install.packages(pkg, repos='http://cran.us.r-project.org')
  }
  
  library(pkg, character.only = TRUE)
  
} # end installPackages()

pkg_list = c("tidyverse", "stringr", "lubridate", "gridExtra")
lapply(pkg_list, install_packages)


# Read in the input file/dataset
v1 <- read_csv("ATP Dataset_2012-01 to 2017-07_Int_V4.csv")

# V2 has date column parsed for month and year components
v2 <- v1 %>% mutate(date_month = month(mdy(Date), label = T, abbr = T),
                                         date_year = year(mdy(Date)))

# Tourn_winners - filter V2 to only include the final match from each tournament
tourn_winners = v2 %>% filter(Round == 'TheFinal')

# Analysis 1 - Monthly Distribution of Tournaments by Surface (Clay, Hard, Grass) 
surface_plot <- tourn_winners  %>%
  ggplot() +
  geom_bar(mapping = aes(x = date_month, fill = Surface), position = "dodge") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  labs(x = "Month", y = "Count", title = "Monthly Distribution of ATP Tournaments by Surface (2012-2017)") +
  facet_wrap(~date_year) 

surface_dist <- grid.arrange(surface_plot + 
                 labs(caption = "Note: Missing Data After July 2017"))

ggsave("surface_distribution.jpg", surface_dist, units = "in", width = 14, height = 8)

# Analysis 2 - Monthly Distribution of Tournaments by Court Type (Indoor/Outdoor) 
court_plot <- tourn_winners %>% 
  ggplot() +
  geom_bar(mapping = aes(x = date_month, fill = Court), position = "dodge") +
  scale_y_continuous(breaks = c(0,2,4,6,8,10)) +
  labs(x = "Month", y = "Count", title = "Monthly Distribution of ATP Tournaments by Court Type (2012-2017)") +
  facet_wrap(~date_year)

court_dist <- grid.arrange(court_plot + 
               labs(caption = "Note: Missing Data After July 2017"))


ggsave("court_distribution.jpg", court_dist, units = "in", width = 14, height = 8)


##### Requires: Player name in the following format: "FirstName MiddleName(optional) LastName"
##### Modifies: Nothing
##### Effects: Finds the player name formatted string as represented in the
#####          dataset to query with in player analyses below
extract_name_components <- function(input_str){
 
  query_idx <- c()
  query_name <- ""
  
  input_name <- str_split(str_to_title(input_str), pattern = " ")
  
  # Extract first and last name from input
  last_name <- input_name[[1]][length(input_name[[1]])]
  first_name <- input_name[[1]][1]
   
  first_initial <- substr(first_name, 1,1)
  query <- str_c(last_name, first_initial)
  
  # Search for Player name in Player 1 column
  query_idx <- grep(query, v2$Player1 )
  
  # If player name not found in Player 1 column, search Player 2 column
  if(length(query_idx) == 0){
   
     query_idx <- grep(search_name, v2$Player2 )
    query_name <- v2$Player2[query_idx[1]]
    
    # If name not found in Player 2 column, return null
    if(length(query_idx) == 0){
      return(NULL)
    }
    
    print(str_c("Query name is ", query_name))
    return(query_name)
  }
  
  query_name <- v2$Player1[query_idx[1]]
  print(str_c("Query name is ", query_name))
  query_name
}


# Analysis 3 - Look at specific player wins by surface

##### Requires: Player's name in the following format: "FirstName MiddleName(optional) LastName"
##### Modifies: Nothing
##### Output: A Player's Match Wins by Surface from 2012-2017
wins_by_surface <- function(player_name){
  
  query <- extract_name_components(player_name)
  
  surface_wins_plot <- v2 %>% filter(Winner == query ) %>% ggplot() + 
    geom_bar(aes(x = date_year, fill = Surface), position = "dodge") +
    scale_x_continuous(breaks = c(2012,2013,2014,2015,2016, 2017)) +
    labs(x = "Year", y = "Match Wins", 
         title = str_c(str_to_title(player_name), "'s Match Wins by Surface (2012-2017)", sep = ""))
  
  grid.arrange(surface_wins_plot + 
                 labs(caption = "Note: Missing Data After July 2017"))
  
}

# Analysis 4 - Rankings History from 2012-2017 for a specific player

##### Requires: Player's name in the following format: "FirstName MiddleName(optional) LastName"
##### Modifies: Nothing
##### Output: A Player's Full Rankings History from 2012-2017
rankings_history_full <- function(player_name){
  
  query = extract_name_components(player_name)
 
  v3 <- v2 %>% filter(Player1 == query | Player2 == query) %>%
    mutate(player_rank = ifelse(Player1 == query, as.integer(Player1_Rank),
                               as.integer(Player2_Rank)))
    
  rankings_plot <- v3 %>% ggplot(aes(x = mdy(Date), y = player_rank)) +
    geom_point(color = "blue") + 
    scale_y_reverse() +
    scale_x_date(date_labels = "%b %y") +
    labs(x = "Time", y = "Singles Ranking", 
         title = str_c("Rankings History for 2012-2017:", str_to_title(player_name), sep = " "))
  
  grid.arrange(rankings_plot + 
                 labs(caption = "Note: Missing Data After July 2017"))
  
}

# Analysis 5 - Top N Rankings History for a specific player
##### Requires: Player's name in the following format: "FirstName MiddleName(optional) LastName"
##### Modifies: Nothing
##### Output: A Player's Rankings History for Top N where N = 10,20,30,50,etc.
rankings_topn<- function(player_name, n){
  
  query = extract_name_components(player_name)
  
  v3 <- v2 %>% filter(Player1 == query | Player2 == query) %>%
    mutate(player_rank = ifelse(Player1 == query, as.integer(Player1_Rank),
                                as.integer(Player2_Rank))) %>% 
    filter(player_rank <= n)
  
  topn_plot <- v3 %>% ggplot(aes(x = mdy(Date), y = player_rank)) +
    geom_point(color = "blue") + 
    scale_y_reverse() +
    scale_x_date(date_labels = "%b %y") +
  labs(x = "Time", y = "Singles Ranking", 
       title = str_c("Top", n, "Rankings History:", str_to_title(player_name), sep = " "))
  
  grid.arrange(topn_plot + 
                 labs(caption = "Note: Missing Data After July 2017"))
  
} 


# Example Plots
ggsave("federer_wins_surface.jpg", plot = wins_by_surface("Roger Federer") , 
       units = "in", width = 8, height = 8 )

ggsave("nadal_wins_surface.jpg", wins_by_surface("Rafael Nadal"),
       units = "in", width = 8, height = 8 )

ggsave("thiem_rankings_history_full.jpg", rankings_history_full("Dominic Thiem"),
       units = "in", width = 8, height = 8 )

ggsave("thiem_rankings_top30.jpg", rankings_topn("Dominic Thiem", 30),
       units = "in", width = 8, height = 8 )



