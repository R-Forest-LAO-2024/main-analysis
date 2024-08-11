

## automatise

tt <- read_csv("data/mock_tree.csv")

tt2 <- tt |>
  mutate(agb = 0.4 * dbh ^ 1.8)

write(tt2, "res/tt2.csv")

if (!("tt2" %in% list.files(" res"))) {
  
  tt <- read_csv("data/mock_tree.csv")
  
  tt2 <- tt |>
    mutate(agb = 0.4 * dbh ^ 1.8)
  
  write(tt2, "res/tt2.csv")
  
}


## User input 

user_reset <-  FALSE


if (!("tt2" %in% list.files(" res")) | user_reset) {
  
  tt <- read_csv("data/mock_tree.csv")
  
  tt2 <- tt |>
    mutate(agb = 0.4 * dbh ^ 1.8)
  
  write(tt2, "res/tt2.csv")
  
}
