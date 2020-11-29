# Plot building -----------------------------------------------------------
team <- "Ajax"


football_filtered <- football %>% subset(team1 == team | team2 == team) %>%
  select(date, league, team1, team2, prob1:probtie) %>%
  mutate(site = ifelse(team == team1, "Home", "Away"),
         opponent = ifelse(team == team1, team2, team1),
         Win = ifelse(site == "Home", prob1, prob2),
         Lose = ifelse(site == "Home", prob2, prob1),
         Draw = probtie,
         team = team) %>% 
  select(-c(team1:probtie)) %>% 
  gather(key = "outcome", value = "probability", Win, Lose, Draw) %>% 
  arrange(date)



ggplot(data = football_filtered,
       mapping = aes(x = date,
                     y = probability,
                     color = outcome)) +
  geom_line() +
  geom_text_repel(aes(label = opponent))



####
plot_ly(data = football_filtered,
        x = ~date,
        y = ~probability,
        hoverinfo = "text",
        text = ~paste("P:", round(probability, 2), "<br>", "Opponent:", opponent)) %>% 
  add_lines(color = ~fct_rev(outcome), colors = c("#66DF90", "#D15656", "#6692DF")) %>% 
  add_markers(symbol = ~factor(site), showlegend = FALSE) %>% 
  layout(xaxis = list(title = "Date"),
         yaxis = list(title = "Probability"),
         title = "Ajax")


plot_ly(data = football_filtered,
        x = ~date,
        y = ~probability,
        symbol = ~fct_rev(outcome)) %>% 
  add_markers(colors = c("#66DF90", "#D15656", "#6692DF"))



football_filtered %>% distinct(date, opponent, .keep_all = TRUE)
