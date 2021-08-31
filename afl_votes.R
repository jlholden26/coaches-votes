##### LOAD THE LIBRARIES
library(tidyverse)
library(rvest)
library(stringr)

##### DEFINE THE FUNCTIONS

# web scrape votes from AFLCA website
get_coaches_votes <- function(season, round, finals){
  
  # awards are different depending on finals
  link_base <- ifelse(finals,
                      "https://aflcoaches.com.au/awards/gary-ayres-award-best-finals-player/leaderboard/",
                      "https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/")
  
  # finish the link depending on the round and season
  link <- paste0(link_base, season, "/", season, "01", sprintf("%02d", round))
  
  # read the link
  print(link)
  html <- read_html(link)
  
  # extract each piece of information - may be primitive
  home.teams <- html_nodes(html, ".pr-md-3.votes-by-match .club_logo") %>% html_attr("title") %>% .[seq(1,length(.),2)]
  away.teams <- html_nodes(html, ".pr-md-3.votes-by-match .club_logo") %>% html_attr("title") %>% .[seq(2,length(.),2)]
  votes <- html_nodes(html, ".pr-md-3.votes-by-match .col-2") %>% html_text %>% str_remove_all("\n") %>% str_remove_all("\t")
  players <- html_nodes(html, ".pr-md-3.votes-by-match .col-10") %>% html_text %>% str_remove_all("\n") %>% str_remove_all("\t")
  
  # arrange the info into a data frame
  df <- data.frame(Season = season, Round = round, Finals = finals,
                   Home.Team = NA, Away.Team = NA, Player.Name = players, Coaches.Votes = votes) %>%
    # split the data frame into matches
    mutate(Match.Id = cumsum(Coaches.Votes=="Votes" & Player.Name == "Player (Club)")) %>%
    # assign home and away teams to each match
    mutate(Home.Team = home.teams[Match.Id],
           Away.Team = away.teams[Match.Id]) %>%
    # remove unnecessary rows/columns
    subset(!(Coaches.Votes=="Votes" & Player.Name == "Player (Club)")) %>%
    select(-Match.Id)
  
  return(df)
  
}

# collate the possible combinations
possible_vote_combinations <- function(df){
  
  # error catching
  if(length(unique(df$Player.Name)) < nrow(df)) stop("Duplicate Player Names")
  if(nrow(df) < 5) stop("Not enough players")
  if(nrow(df) > 10) stop("Too many players")
  
  # start at the bottom for fewer options & reset row names
  df <- arrange(df, Coaches.Votes) %>% `rownames<-`(NULL)
  
  # template for votes
  template <- data.frame(Votes = c(5,4,3,2,1), C1 = NA, C2 = NA)
  
  # master votes list to add to
  master_votes <- list(template)
  
  # loop through each player and try differing combinations
  loop <- split(df, 1:nrow(df)) %>%
    lapply(function(row){
      
      # assign variables
      cv <- row$Coaches.Votes %>% as.numeric
      nm <- row$Player.Name
      min_cv <- max(0, cv - 5)
      max_cv <- min(cv, 5)
      
      # create list of options for this player
      options <- list()
      
      # loop through current eligible options
      for(opt in master_votes){
      
        # loop through possible vote combinations for that player
        for(n in seq(min_cv, max_cv)){
          
          # if that combination is blocked out for this eligible option, skip to the next option
          if(n == 0){
            if(!is.na(subset(opt, Votes == cv - n)$C2)) next
          } else if(cv == n){
            if(!is.na(subset(opt, Votes == n)$C1)) next
          } else if(!is.na(subset(opt, Votes == n)$C1) | !is.na(subset(opt, Votes == cv - n)$C2)) next
          
          # otherwise, update this option with this vote combination for this player
          opt_new = opt
          opt_new[opt_new$Votes==n, "C1"] <- nm
          opt_new[opt_new$Votes==(cv-n), "C2"] <- nm
          options <- c(options, list(opt_new))
          
        }
        
      }
      
      # update the master options list
      assign("master_votes", options, envir = parent.frame(n=2))
      
      # final list of combinations - only return if it's the last name on the list
      if(rownames(row) == nrow(df)) return(options)
      
    })
  
  # take the final item in the loop (last name on the list)
  final_outcomes <- loop[[nrow(df)]]
  
  # drop duplicate options (C1 = C2 and C2 = C1)
  # loop through outcomes
  for(i in 1:(length(final_outcomes) - 1)){
    
    # skip if not possible
    if(i>=length(final_outcomes)) next
    
    # loop through pairing outcomes
    for(j in (i+1):length(final_outcomes)){
      
      # skip if not possible
      if(j>length(final_outcomes)) next
      
      # remove duplicate if it matches
      if(sum(final_outcomes[[i]]$C1 != final_outcomes[[j]]$C2) + sum(final_outcomes[[i]]$C2 != final_outcomes[[j]]$C1) == 0){
        final_outcomes[[j]] <- NULL
      }
      
    }
    
  }
  
  return(final_outcomes)
  
}

##### RUN THE CODE

### Single Match

# parameters
season <- 2021
round <- 23 + 1 # For finals needs to be the last round + finals week
finals <- TRUE
team <- "Western Bulldogs" # should belong to Adelaide Crows, Brisbane Lions, Carlton, Collingwood, Essendon, Fremantle, Geelong Cats, Gold Coast Suns, GWS Giants, Hawthorn, Melbourne, North Melbourne, Port Adelaide, Richmond, St Kilda, Sydney Swans, West Coast Eagles, Western Bulldogs

# output
get_coaches_votes(season, round, finals) %>%
  subset(Home.Team == team | Away.Team == team) %>%
  possible_vote_combinations

### Scrape all coaches votes data and run
all_coaches_votes <- expand.grid(Season = 2007:2021, Round = 1:27, Finals = c(F,T)) %>%
  as.data.frame %>%
  # exclude obvious impossibilities
  subset(!(
    (Season<2018 & Finals) |
      (Round<19 & Finals) |
      (Round>23 & !Finals)
  )) %>%
  split(1:nrow(.)) %>%
  # apply function to each round
  lapply(function(row){
    try(get_coaches_votes(row$Season, row$Round, row$Finals))
  })

# remove errors
all_coaches_votes[sapply(all_coaches_votes, typeof)=="character"] <- NULL

# create data frame
all_coaches_votes <- do.call(rbind, all_coaches_votes)

# parameters
season <- 2021
round <- 23 + 1 # For finals needs to be the last round + finals week
finals <- TRUE
team <- "Western Bulldogs" # should belong to Adelaide Crows, Brisbane Lions, Carlton, Collingwood, Essendon, Fremantle, Geelong Cats, Gold Coast Suns, GWS Giants, Hawthorn, Melbourne, North Melbourne, Port Adelaide, Richmond, St Kilda, Sydney Swans, West Coast Eagles, Western Bulldogs

all_coaches_votes %>%
  subset(Season==season & Round==round & Finals==finals & (Home.Team == team | Away.Team == team)) %>%
  possible_vote_combinations

### Manual Example

df <- data.frame(Player.Name = c("Tom Liberatore",
                                 "Jack Macrae",
                                 "Marcus Bontempelli",
                                 "Cody Weightman",
                                 "Darcy Parish",
                                 "Aaron Naughton",
                                 "Jordan Ridley"),
                 Coaches.Votes = c(7, 6, 5, 5, 4, 2, 1))

possible_vote_combinations(df)