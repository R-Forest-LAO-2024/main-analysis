
##
## TREEPLOT ######
##

## Read data from the XLSX file
treeplot_init <- readxl::read_xlsx(path_datafile, sheet = "data")

treeplot_lc <- readxl::read_xlsx(path_datafile, sheet = "Plot", range = cell_cols("A:D")) |>
  rename(
    plot_no = "plot_info/plot_code_nmbr",
    treeplot_id = "plot_info/sub_plot", 
    lc_type = "lc_data/lc_type",
    lc_class = "lc_class/lc_class"
  ) |>
  mutate(
    treeplot_id = case_when(
      str_length(treeplot_id) == 2 ~ paste0("0", treeplot_id),
      str_length(treeplot_id) == 3 ~ treeplot_id,
      TRUE ~ NA_character_
    )
  )


##
## TREE ######
##

## MANUAL APPROACH ######
## + Load the data raw
tree_init1 <- readxl::read_xlsx(path = path_datafile, sheet = "tree_data_nest1") 
tree_init2 <- readxl::read_xlsx(path = path_datafile, sheet = "tree_data_nest2") 

## + Correct column name error in tree_data_nest2
err_colname <- "survey/measure/tree_data_nest2/tree_data_nest2_rep/tree_dead_nest2_cl2_tall/t_dead_height_dbh_nest2_short" 
err_pos <- match(err_colname, names(tree_init2))
names(tree_init2)[err_pos] <- "t_dead_height_dbh_nest2_tall"

## + Add filename to data to keep track of origin file
## + Keep the trailing part of all "/" in column name
## + Remove the digit after all "nest1" or "nest2"
## + remame "_index" as it represent the ONA unique treeplot ID.
tree_tmp1 <- tree_init1 |>  
  mutate(filename = "tree_data_nest1") |>
  rename_with(str_remove, pattern = ".*/") |>
  rename_with(str_remove, pattern = "_nest1") |>
  rename(
    ONA_treeplot_id = `_parent_index`
  )

tree_tmp2 <- tree_init2 |>  
  mutate(filename = "tree_data_nest2") |>
  rename_with(str_remove, pattern = ".*/") |>
  rename_with(str_remove, pattern = "_nest2") |>
  rename(
    ONA_treeplot_id = `_parent_index`
  )

## Combine the two datasets by binding rows
tree_init <- bind_rows(tree_tmp1, tree_tmp2)

rm(tree_tmp1, tree_tmp2, tree_init1, tree_init2)


## AUTOMATIC APPROACH ######

## Check the name of all tree sheets starting witn "tree_data_nest"
vec_tree <- readxl::excel_sheets(path_datafile) |> str_subset(pattern = "tree_data_nest")

## PREPARE INITAL TREE TABLE BY ROW BINDING ALL SHEETS STARTING WITH "tree_data_nest"
tree_init <- map(vec_tree, function(x){
  
  ## + Read XL tab
  tt <- readxl::read_xlsx(path_datafile, x) 
  
  ## Correct typo in nest2 column name
  err_colname <- "survey/measure/tree_data_nest2/tree_data_nest2_rep/tree_dead_nest2_cl2_tall/t_dead_height_dbh_nest2_short" 
  if (err_colname %in% names(tt)){
    err_pos <- match(err_colname, names(tt))
    names(tt)[err_pos] <- "t_dead_height_dbh_nest2_tall"
  }
  
  ## Simplify column names and harmonize ONA treeplot IDs
  tt |>
    mutate(filename = x) |>
    rename_with(str_remove, pattern = ".*/") |>
    rename_with(str_remove, pattern = "_nest[0-9]") |>
    rename(
      ONA_treeplot_id = `_parent_index`
    )
  
}) |> list_rbind()
tree_init



# |>
#   select(
#     #plot_no = "plot_info/plot_code_nmbr", 
#     treeplot_id = "plot_info/sub_plot", 
#     lc_type = "lc_data/lc_type",
#     lc_class = "lc_class/lc_class"
#   ) |>
#   mutate(
#     treeplot_id = case_when(
#       str_length(treeplot_id) == 2 ~ paste0("0", treeplot_id),
#       str_length(treeplot_id) == 3 ~ treeplot_id,
#       TRUE ~ NA_character_
#     )
#   )
# treeplot_lc

# treeplot_index <- data_all |> 
#   select(
#     crew_lead = "plot_info/crew_lead",
#     ONA_treeplot_id = "_index",
#     plot_no = "plot_info/plot_code_nmbr", 
#     treeplot_no = "plot_info/sub_plot",
#     treeplot_gps_lat = "plot_GPS/_GPS_latitude",
#     treeplot_gps_lon = "plot_GPS/_GPS_longitude",
#   ) |>
#   filter(crew_lead != "QC") |>
#   mutate(
#     plot_no = if_else(plot_no == 135, 13, plot_no),
#     plot_id = case_when(
#       plot_no < 10 ~ paste0("0", plot_no),
#       plot_no < 100 ~ as.character(plot_no),
#       TRUE ~ NA_character_
#     ),
#     treeplot_id = paste0(plot_id, treeplot_no)
#   ) |>
#   select(plot_id, plot_no, treeplot_id, treeplot_no, ONA_treeplot_id, treeplot_gps_lat, treeplot_gps_lon)
# 
# treeplot_info <- treeplot_index |>
#   inner_join(treeplot_lc, by = "treeplot_id") |>
#   arrange(plot_no, treeplot_no)