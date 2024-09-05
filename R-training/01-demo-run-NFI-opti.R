

library(tictoc)

tic()

check_plot     <- "plot.csv" %in% list.files("data")
check_treeplot <- "treeplot.csv" %in% list.files("data")
check_tree     <- "tree-live.csv" %in% list.files("data")
check_dw       <- "standing-deadwood.csv" %in% list.files("data")
check_stump    <- "stump.csv" %in% list.files("data")
check_all <- check_plot & check_treeplot & check_tree & check_dw & check_stump
usr_reset <- FALSE


source("R-analysis/00-setup.R", local = TRUE)

source("R-analysis/00-functions.R", local = TRUE)

if (!check_all | usr_reset) {
  
  source("R-analysis/01-load-tables.R", local = TRUE)
  
  source("R-analysis/01-create-objects.R", local = TRUE)
  
  source("R-analysis/02a-harmo-treeplot.R", local = TRUE)
  
  source("R-analysis/02b-harmo-plot.R", local = TRUE)
  
  source("R-analysis/02c-harmo-tree.R", local = TRUE)
  
  source("R-analysis/02d-create-dw.R", local = TRUE)
  
  source("R-analysis/02e-create-stump.R", local = TRUE)
  
  source("R-analysis/02f-save-tables.R", local = TRUE)

}

source("R-analysis/03-read-harmo.R", local = TRUE)

source("R-analysis/05-calc-agb.R", local = TRUE)

source("R-analysis/06-agg-treeplot.R", local = TRUE)

source("R-analysis/07-agg-plot.R", local = TRUE)

source("R-analysis/08-agg-ftype.R", local = TRUE)

toc()
