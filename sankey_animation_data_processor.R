###

fnp <- "sankey_animation_data.csv"

tib <- as_tibble(read.csv(fnp))

# build coords lookup
y2_strings <- c("Y2 Status A","Y2 Status B","Y2 Status C")

y2_ints <- c(
  animated_sankey_master_list$right_statuses_y$y2a_to,
  animated_sankey_master_list$right_statuses_y$y2b_to,
  animated_sankey_master_list$right_statuses_y$y2c_to
)

left_pos_tib <- tibble(
  y1=c(
    rep("Y1 Status A",3),
    rep("Y1 Status B",3),
    rep("Y1 Status C",3),
    rep("Y1 Status D",3),
    rep("Y1 Status E",3),
    rep("Y1 Status F",3),
    rep("Y1 Status G",3),
    rep("Y1 Status H",3)
  ),
  y2=rep(y2_strings,8),
  from=c(
    rep(8.0,3),
    rep(7.0,3),
    rep(6.0,3),
    rep(5.0,3),
    rep(4.0,3),
    rep(3.0,3),
    rep(2.0,3),
    rep(1.0,3)
  ),
  to=rep(y2_ints,8)
)

tib <- merge(
  tib,
  left_pos_tib,
  by=c("y1","y2")
)

# order by from/to desc, mutate row number

sank_tib <- tib %>% 
  arrange(-from,-to) %>% 
  mutate(id = row_number(),
         y1 = as.character(y1)) %>%
  rename(status = y1) %>%
  select(from,to,id,status) 

sank_tib <- as_tibble(sank_tib)
