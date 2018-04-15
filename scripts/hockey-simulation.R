library(dplyr)
library(elo)
library(rbenchmark)

hockey = readr::read_csv("~/Dropbox/data/mn-hockey-boys-2018-03-09.csv") %>%
  filter(season != "State Tournament")
hockey




elo_reg_season = elo.run(score(home_score, visitor_score) ~ home + 
  visitor + k(15*log(abs(home_score - visitor_score) + 1)), 
  data = hockey)


# Enter teams in rank order
team_1 = "Hermantown"
team_2 = "Mahtomedi"
team_3 = "Orono"
team_4 = "Alexandria"
team_5 = "Thief River Falls"
team_6 = "Litchfield/Dassel-Cokato"
team_7 = "Mankato East"
team_8 = "Monticello"

# Set up empty vector to store winner in
champion = rep(NA, 100)


system.time(
  for(i in 1:1000){
    
    ### SIMULATE THE QUARTEFINALS
    
    # Predict Game 1 winner: team_1 vs. team_8
    p_game_1 = predict(elo_reg_season, data.frame(home = team_1, visitor = team_8))
    w_game_1 = ifelse(runif(1, min = 0, max = 1) <= p_game_1, team_1, team_8)
    champion[i] = w_game_1
    
    # Predict Game 2 winner: team_4 vs. team_4
    p_game_2 = predict(elo_reg_season, data.frame(home = team_4, visitor = team_5))
    w_game_2 = ifelse(runif(1, min = 0, max = 1) <= p_game_2, team_4, team_5)
    
    # Predict Game 3 winner: team_3 vs. team_6
    p_game_3 = predict(elo_reg_season, data.frame(home = team_3, visitor = team_6))
    w_game_3 = ifelse(runif(1, min = 0, max = 1) <= p_game_3, team_3, team_6)
    
    # Predict Game 4 winner: team_2 vs. team_7
    p_game_4 = predict(elo_reg_season, data.frame(home = team_2, visitor = team_7))
    w_game_4 = ifelse(runif(1, min = 0, max = 1) <= p_game_4, team_2, team_7)
    

    ### SIMULATE THE SEMIFINALS

    # Predict Game 5 winner: winner Game 1 vs. winner Game 2
    p_game_5 = predict(elo_reg_season, data.frame(home = w_game_1, visitor = w_game_2))
    w_game_5 = ifelse(runif(1, min = 0, max = 1) <= p_game_5, w_game_1, w_game_2)

    # Predict Game 6 winner: winner Game 3 vs. winner Game 4
    p_game_6 = predict(elo_reg_season, data.frame(home = w_game_4, visitor = w_game_3))
    w_game_6 = ifelse(runif(1, min = 0, max = 1) <= p_game_6, w_game_4, w_game_3)


    ### SIMULATE THE FINALS

    # Predict Game 5 winner: winner Game 1 vs. winner Game 2
    p_game_7 = predict(elo_reg_season, data.frame(home = w_game_5, visitor = w_game_6))
    w_game_7 = ifelse(runif(1, min = 0, max = 1) <= p_game_7, w_game_5, w_game_6)

    champion[[i]] = w_game_7
    
  }
  )


elo_reg_season %>%
  map(~predict.elo.run(., data.frame(home = team_1, visitor = team_8)))
  
  predict(data.frame(home = team_1, visitor = team_8)) %>%
  mutate(w_game_1 = ifelse(runif(1, min = 0, max = 1) <= p_game_1, team_1, team_8))


library(purrr)

x = sti %>% 
  select(challenging, X7_3, X8_1)  %>%
  map(~wtd.chi.sq(var1 = ., var2 = sti$group, weight = sti$Weight))



  

quarterfinals = tibble(
  home = c(team_1, team_4, team_3, team_2),
  visitor = c(team_8, team_5, team_6, team_7),
  prob_home = c(
    predict(elo_reg_season, data.frame(home = team_1, visitor = team_8)),
    predict(elo_reg_season, data.frame(home = team_4, visitor = team_5)),
    predict(elo_reg_season, data.frame(home = team_3, visitor = team_6)),
    predict(elo_reg_season, data.frame(home = team_2, visitor = team_7))
    )
  ) %>%
  mutate(
    winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
    game = "Quarterfinals"
    )
  
semifinals = tibble(
  home = c(quarterfinals$winner[1], quarterfinals$winner[4]),
  visitor = c(quarterfinals$winner[2], quarterfinals$winner[3]),
  prob_home = c(
    predict(elo_reg_season, data.frame(home = quarterfinals$winner[1], visitor = quarterfinals$winner[2])),
    predict(elo_reg_season, data.frame(home = quarterfinals$winner[4], visitor = quarterfinals$winner[3]))
  )
) %>%
  mutate(
    winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
    game = "Semifinals"
  )


finals = tibble(
  home = semifinals$winner[1],
  visitor = semifinals$winner[2],
  prob_home = c(
    predict(elo_reg_season, data.frame(home = semifinals$winner[1], visitor = semifinals$winner[2]))
   )
) %>%
  mutate(
    winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
    game = "Final"
  )

quarterfinals
semifinals
finals




#############################################
  
my_qf = function(x){
  quarterfinals = tibble(
    home = c(team_1, team_4, team_3, team_2),
    visitor = c(team_8, team_5, team_6, team_7),
    prob_home = c(
      predict(elo_reg_season, data.frame(home = team_1, visitor = team_8)),
      predict(elo_reg_season, data.frame(home = team_4, visitor = team_5)),
      predict(elo_reg_season, data.frame(home = team_3, visitor = team_6)),
      predict(elo_reg_season, data.frame(home = team_2, visitor = team_7))
    )
  ) %>%
  mutate(
    winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
    game = "Quarterfinals"
  )
  
  return(quarterfinals)
}


my_sf = function(x){
  
  t1 = X$QF, "winner"), 1)
  t2 = map_chr(map(X$QF, "winner"), 2)
  t3 = map_chr(map(X$QF, "winner"), 3)
  t4 = map_chr(map(X$QF, "winner"), 4)
  
  semifinals = tibble(
    home = c(t1, t2),
    visitor = c(t3, t4),
    prob_home = c(
      predict(elo_reg_season, data.frame(home = t1, visitor = t2)),
      predict(elo_reg_season, data.frame(home = t3, visitor = t4))
    )
  ) %>%
    mutate(
      winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
      game = "Semifinals"
    )
  
  return(semifinals)
}


my_f = function(x){
  finals = tibble(
    home = semifinals$winner[1],
    visitor = semifinals$winner[2],
    prob_home = c(
      predict(elo_reg_season, data.frame(home = semifinals$winner[1], visitor = semifinals$winner[2]))
    )
  ) %>%
    mutate(
      winner = ifelse(runif(1, min = 0, max = 1) <= prob_home, home, visitor),
      game = "Final"
    )
  
  return(finals)
}



system.time()

X = tibble(Trial = paste0("Trial_", 1:2)) %>%
  mutate(
    QF = map(Trial, ~my_qf(.))
    ) %>%
  mutate(
    SF = map(Trial, ~my_sf(.))
  ) %>%
  mutate(
    F = map(Trial, ~my_f(.))
  ) %>%
  mutate(champion = map_chr(F, "winner"))
X

X$QF[[1]]
X$SF[[1]]
X$F[[1]]


x = sti %>% 
  select(challenging, X7_3, X8_1)  %>%
  map(~wtd.chi.sq(var1 = ., var2 = sti$group, weight = sti$Weight))
