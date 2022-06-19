library(scales)
library(tibble)
library(readr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
dataset = read.csv("DataScienceProjectDataset.csv")
as_tibble(dataset)

full_collection = nrow(dataset) #740 stickers

#q1
shiny_subset = dataset %>% 
  filter(SHINY == "YES")

non_shiny_subset = dataset %>% 
  filter(SHINY == "NO")

nr_of_shinies = nrow(shiny_subset) #124 shiny stickers
nr_of_nonshinies = nrow(non_shiny_subset) #616 non-shiny stickers

decimal_collection = 250 / full_collection
percentage_of_collection = percent(decimal_collection , accuracy = .01)
percentage_of_collection

expected_shinies = ceiling(decimal_collection * nr_of_shinies)
expected_shinies #42 expected shinies pulled
expected_nonshinies = ceiling(decimal_collection * nr_of_nonshinies)
expected_nonshinies #209 expected non-shinies pulled

min_shinies = expected_shinies - floor(expected_shinies * 0.2)
min_shinies #34 minimum number of expected shinies pulled

max_shinies = expected_shinies + floor(expected_shinies * 0.2)
max_shinies #50 maximum number of expected shinies pulled

#The rate of the shinies I pulled will be considered normal if I have pulled between 34 and 50 shiny stickers.

shinies_pulled = shiny_subset %>% 
  filter(TIMES.PULLED != 0)

nr_of_shinies_pulled = nrow(shinies_pulled)
nr_of_shinies_pulled #43 pulled shinies

shinyplot <- c(min_shinies,expected_shinies,max_shinies)
shinyplot <- data.frame(shinyplot)
plot(shinyplot, type="b", main="Interval of Expected Nr. of Shinies Pulled")
mtext("Min Expected", side = 1, adj = 0, padj = -2)
mtext("Max Expected", side = 1, adj = 1, padj = -2)
mtext("Expected", side = 1, padj = -5)

shinyplot2 <- c(min_shinies,expected_shinies,nr_of_shinies_pulled,max_shinies)
shinyplot2 <- data.frame(shinyplot2)
plot(shinyplot2, type="b", main="Interval of Expected Nr. of Shinies Pulled")
mtext("34", side = 1, adj = 0, padj = -2)
mtext("50", side = 1, adj = 1, padj = -2)
mtext("42", side = 1, padj = -2)
mtext("Min Expected", side = 1, adj = 0, padj = -12)
mtext("Max Expected", side = 1, adj = 1, padj = -12)
mtext("Expected", side = 1, padj = -10)
mtext("Pulled", side = 1, adj = 0.58,  padj = -7, col = "Red")
mtext("43", side = 1, adj = 0.58,  padj = -2, col = "Red")

just_players = dataset %>%
  filter(TYPE == "PLAYER") %>%
  select (TYPE, NAME, TEAM, NATION)
just_players

no_of_players = nrow(just_players)
no_of_players #440 serie a players

players_pulled = just_players %>% 
                filter(TIMES.PULLED != 0)
players_pulled

no_of_players_pulled = nrow(players_pulled)
no_of_players_pulled  #126 serie a players pulled

expected_no_of_players_pulled = ceiling(no_of_players * decimal_collection)
expected_no_of_players_pulled #149 serie a players expected pulled

min_players_pulled = expected_no_of_players_pulled - floor(expected_no_of_players_pulled * 0.2)
min_players_pulled #120 minimum number of expected players pulled

max_players_pulled = expected_no_of_players_pulled + floor(expected_no_of_players_pulled * 0.2)
max_players_pulled #178 maximum number of expected players pulled

shinyplot3 <- c(min_players_pulled,no_of_players_pulled,expected_no_of_players_pulled,max_players_pulled)
shinyplot3 <- data.frame(shinyplot3)
plot(shinyplot3, type="b", main="Interval of Expected Nr. of Serie A Players Pulled")
mtext("120", side = 1, adj = 0, padj = -2)
mtext("178", side = 1, adj = 1, padj = -2)
mtext("149", side = 1, padj = -2)
mtext("Pulled", side = 1, adj = 0.1,  padj = -5, col = "Red")
mtext("126", side = 1, adj = 0.1,  padj = -2, col = "Red")

#q2

stickers_by_team = dataset %>%
  filter(TEAM != "ND") %>% 
  count(TEAM, sort = TRUE)
stickers_by_team

stickers_by_team2 = stickers_by_team %>%
  mutate(expected = ceiling(n * decimal_collection)) %>%
  mutate(min_expected = ifelse(expected <= 5,expected - 1 - floor(expected * 0.2),expected - floor(expected * 0.2)))%>%
  mutate(max_expected = ifelse(expected <= 5,expected + 1 + floor(expected * 0.2),expected + floor(expected * 0.2)))%>%
  select(TEAM, min_expected, expected, max_expected) %>%
  arrange(TEAM)
stickers_by_team2

pulled_by_team = dataset %>%
  filter(TEAM != "ND") %>%
  count(TEAM, TIMES.PULLED, sort = TRUE) %>%
  mutate(SUM_BY_TEAM = TIMES.PULLED * n) %>%
  group_by(TEAM)%>%
  summarise(SUM_BY_TEAM = sum(SUM_BY_TEAM)) %>%
  arrange(desc(SUM_BY_TEAM))
pulled_by_team

team_join = inner_join(stickers_by_team2,pulled_by_team)

ggplot(team_join, aes(x=TEAM, y=SUM_BY_TEAM)) + 
  geom_bar(stat = "identity") +
  coord_flip() + theme(text = element_text(size = 5)) +
  geom_line(aes(x = TEAM, y = min_expected), size = 0.5, color="orange", group = 1) +
  geom_line(aes(x = TEAM, y = max_expected), size = 0.5, color="red", group = 1)

#q3

nationalities_of_players = just_players %>%
  count(NATION, sort = TRUE)
nationalities_of_players

at_least_five = nationalities_of_players %>%
  filter(n >= 5)
at_least_five

stickers_by_nation = at_least_five %>%
  mutate(expected_nation = ceiling(n * decimal_collection)) %>%
  mutate(min_expected_nation = ifelse(expected_nation <= 5,expected_nation - 1 - floor(expected_nation * 0.2),expected_nation - floor(expected_nation * 0.2)))%>%
  mutate(max_expected_nation = ifelse(expected_nation <= 5,expected_nation + 1 + floor(expected_nation * 0.2),expected_nation + floor(expected_nation * 0.2)))%>%
  select(NATION, min_expected_nation, expected_nation, max_expected_nation) %>%
  arrange(NATION)
stickers_by_nation

pulled_by_nation = dataset %>%
  filter(TEAM != "ND") %>%
  filter(NATION != "ND") %>%
  count(NATION, TIMES.PULLED, sort = TRUE) %>%
  mutate(SUM_BY_NATION = TIMES.PULLED * n) %>%
  group_by(NATION)%>%
  summarise(SUM_BY_NATION = sum(SUM_BY_NATION)) %>%
  arrange(desc(SUM_BY_NATION))
pulled_by_nation

nation_join = inner_join(stickers_by_nation,pulled_by_nation)
nation_join

ggplot(nation_join, aes(x=NATION, y=SUM_BY_NATION)) + 
  geom_bar(stat = "identity") +
  coord_flip() + theme(text = element_text(size = 5)) +
  geom_line(aes(x = NATION, y = min_expected_nation), size = 0.5, color="orange", group = 1) +
  geom_line(aes(x = NATION, y = max_expected_nation), size = 0.5, color="red", group = 1)

no_of_italian_players = nrow(just_players %>%
  filter(NATION == "ITALY"))

no_of_italian_players #157 number of italian players

expected_italians = ceiling(decimal_collection * no_of_italian_players)
expected_italians #54 expected ialian players pulled

min_italians_pulled = expected_italians - floor(expected_italians * 0.2)
min_italians_pulled #44 minimum number of expected ialian players pulled

max_italians_pulled = expected_italians + floor(expected_italians * 0.2)
max_italians_pulled #64 maximum number of expected italian players pulled

no_of_foreigner_players = nrow(just_players %>%
filter(NATION != "ITALY"))
no_of_foreigner_players #283 foreigner players

expected_foreigners = ceiling(decimal_collection * no_of_foreigner_players)
expected_foreigners #96 expected foreigner players pulled

min_foreigners_pulled = expected_foreigners - floor(expected_foreigners * 0.2)
min_foreigners_pulled #77 minimum number of expected foreigner players pulled

max_foreigners_pulled = expected_foreigners + floor(expected_foreigners * 0.2)
max_foreigners_pulled #115 maximum number of expected foreigner players pulled

italian_players_pulled = dataset %>% 
  filter(TIMES.PULLED != 0 & NATION == "ITALY") %>%
  summarise(PULLED = sum(TIMES.PULLED))
italian_players_pulled #56 italians pulled

foreigner_players_pulled = dataset %>% 
  filter(TIMES.PULLED != 0 & NATION != "ITALY" & NATION != "ND") %>%
  summarise(PULLED = sum(TIMES.PULLED))
foreigner_players_pulled #100 foreigners pulled

italians = data.frame(NATION = "ITALY",
                      MIN_EXPECTED = min_italians_pulled,
                      EXPECTED = expected_italians,
                      MAX_EXPECTED = max_italians_pulled,
                      PULLED = italian_players_pulled)
foreigners = data.frame(NATION = "FOREIGN",
                      MIN_EXPECTED = min_foreigners_pulled,
                      EXPECTED = expected_foreigners,
                      MAX_EXPECTED = max_foreigners_pulled,
                      PULLED = foreigner_players_pulled )

ita_foreigner_table = full_join(italians,foreigners)
ita_foreigner_table

ggplot(ita_foreigner_table, aes(x=NATION, y=PULLED)) + 
  geom_bar(stat = "identity") +
  coord_flip() + theme(text = element_text(size = 5)) +
  geom_line(aes(x = NATION, y = MIN_EXPECTED), size = 0.5, color="orange", group = 1) +
  geom_line(aes(x = NATION, y = MAX_EXPECTED), size = 0.5, color="red", group = 1)