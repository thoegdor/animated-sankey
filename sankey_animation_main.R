rm(list=ls())

# wouldn't be possible without: https://www.hvitfeldt.me/blog/recreate-sankey-flow-chart/

library(tidyverse)
library(gganimate) 
library(extrafont)
#font_import()
#loadfonts(device = "win")

source("sankey_animation_vars_funcs.R")

### source below to get the main dataset - will be accessible as `sank_tib`

source("sankey_animation_data_processor.R")

# generate coords for scatter points
point_data <- map_df(
  
  seq_len(nrow(sank_tib)), 
  ~ sigmoid(
    x_from = 0,
    x_to = 1, 
    y_from= as.numeric(sank_tib[.x, 1]), 
    y_to = as.numeric(sank_tib[.x, 2])
  ) %>%
  mutate(
    frame =  row_number() + .x,
    y = y + runif(1, -0.1, 0.1),
    id = .x
  ) %>%
  bind_cols(
    bind_rows(
      replicate(
        animated_sankey_master_list$n_scatter_coords, 
        sank_tib[.x, -(1:3)], simplify = FALSE
        )
     )
   )
) %>% mutate(x=round(x,2))

### start bar work

bar_data <- point_data %>% 
  
  group_by(id) %>%
  mutate(max_time = max(frame))%>% 
  select(id,status,max_time) %>% unique() %>% 
  left_join(sank_tib %>% select(id,to),by="id") %>%
  mutate(destination = ifelse(to==animated_sankey_master_list$right_statuses_y$y2a_to,"Y2 Status A",
                              ifelse(to==animated_sankey_master_list$right_statuses_y$y2b_to,"Y2 Status B",
                                     "Y2 Status C"))) %>% 
  ungroup() %>%
  select(max_time,status,destination) 

bar_data <- bar_data %>% 
  group_by(max_time,destination) %>% 
  summarize(
    y1a_count=sum(status=="Y1 Status A"),
    y1b_count=sum(status=="Y1 Status B"),
    y1c_count=sum(status=="Y1 Status C"),
    y1d_count=sum(status=="Y1 Status D"),
    y1e_count=sum(status=="Y1 Status E"),
    y1f_count=sum(status=="Y1 Status F"),
    y1g_count=sum(status=="Y1 Status G"),
    y1h_count=sum(status=="Y1 Status H")
  ) %>% 
  ungroup()

### bar viz needs all possible combinations for each frame
# generate those below

bar_data <- bar_data %>% 
  complete(destination,
           nesting(max_time),
           fill=list(y1a_count=0,
                     y1b_count=0,
                     y1c_count=0,
                     y1d_count=0,
                     y1e_count=0,
                     y1f_count=0,
                     y1g_count=0,
                     y1h_count=0))

bar_data <- bar_data %>%
  arrange(max_time) %>%
  group_by(destination) %>%
  mutate(
    y1a_count=cumsum(y1a_count),
    y1b_count=cumsum(y1b_count),
    y1c_count=cumsum(y1c_count),
    y1d_count=cumsum(y1d_count),
    y1e_count=cumsum(y1e_count),
    y1f_count=cumsum(y1f_count),
    y1g_count=cumsum(y1g_count),
    y1h_count=cumsum(y1h_count)
  ) %>% 
  ungroup()

# reshaping
bar_data <- bar_data %>% 
  gather(variable,
         value,
         c("y1a_count","y1b_count",
           "y1c_count","y1d_count",
           "y1e_count","y1f_count",
           "y1g_count","y1h_count")) %>%
  rename(status=variable,
         val=value,
         frame=max_time)

# need to add a zero row for on-track/y1a_count/frame=(however many scatter coords there are)
# before adding coords. Vals don't really matter here

bar_data <- bar_data %>% 
  union(tibble(destination="Y2 Status A",
               frame=animated_sankey_master_list$n_scatter_coords,
               status="y1a_count",
               val=0)
  )
### intended cols: frame, destination, status, x1val, x2val, y1val, y2val
# add static coords 
bar_data <- bar_data %>% 
  mutate( # 
    x1=animated_sankey_master_list$right_statuses_x$x_min,
    x2=animated_sankey_master_list$right_statuses_x$x_max,
    y1=ifelse(destination=="Y2 Status A",
              animated_sankey_master_list$right_statuses_y$y2a_y_start,
       ifelse(destination=="Y2 Status B",
              animated_sankey_master_list$right_statuses_y$y2b_y_start,
       animated_sankey_master_list$right_statuses_y$y2c_y_start)))

# manipulate dynamic coords
# bar heights won't be more than 1, so
# need to generate seq of length N from y1 val to y1 val + 1
# where N represents the number of persons of all statuses 
# in a destination

n_persons_destination <- bar_data %>% 
  group_by(destination,status) %>% 
  summarize(max_val=max(val)) %>% 
  group_by(destination) %>% 
  summarize(n_persons = sum(max_val)) %>% 
  ungroup()

## apply custom func below

coords <- bind_rows(
  
  lapply(
    animated_sankey_master_list$bar_coord_list,
    generate_coords,
    n_groups = length(unique(sank_tib$status))
  )
  
)

# join on bar data

bar_data <- merge(
  
  bar_data,
  coords,
  by=c("status","val","destination"),
  all.x=T
  
)

## y1 needs to change too by count group
# functions similar to the above 

next_data <- bar_data %>% 
  group_by(status,destination) %>% 
  summarize(max_y2=max(y2)) %>% 
  mutate(next_status=ifelse(status=="y1a_count","y1b_count",
                     ifelse(status=="y1b_count","y1c_count",
                     ifelse(status=="y1c_count","y1d_count",
                     ifelse(status=="y1d_count","y1e_count",
                     ifelse(status=="y1e_count","y1f_count",
                     ifelse(status=="y1f_count","y1g_count",
                     ifelse(status=="y1g_count","y1h_count",NA_character_)))))))) %>%
  filter(!is.na(next_status)) %>%
  bind_rows(tibble(status=rep("y1a_count",3), # 
                   destination=c("Y2 Status A", # 
                                 "Y2 Status B",
                                 "Y2 Status C"),
                   max_y2=0,
                   next_status=rep("y1a_count",3))) %>% # 
  ungroup() %>%
  select(next_status,destination,max_y2) 

bar_data <- bar_data %>% 
  left_join(next_data,by=c("status"="next_status",
                           "destination"="destination"))

bar_data <- bar_data %>% 
  mutate(max_y2=ifelse(max_y2>y1,max_y2,y1)) %>%
  select(status,val,destination,frame,x1,x2,max_y2,y2)

# since val isn't being used anymore, need to zero out all
# bars before they have appeared...

bar_data <- bar_data %>% 
  group_by(destination,status) %>% 
  mutate(y2=ifelse(val==0,max_y2,y2)) %>%
  select(status,val,destination,frame,x1,x2,max_y2,y2) %>%
  ungroup() %>% 
  mutate(frame=as.integer(frame))

# custom colors so plots can be added together with different aes mappings
bar_data <- bar_data %>% 
  mutate(color_col=ifelse(status=="y1a_count","grey30", # 
                   ifelse(status=="y1b_count","grey35",
                   ifelse(status=="y1c_count","grey40",
                   ifelse(status=="y1d_count","grey45",
                   ifelse(status=="y1e_count","grey50",
                   ifelse(status=="y1f_count","grey55",
                   ifelse(status=="y1g_count","grey60","grey65")))))))
  )

# impute all of the <[n_scatter_coords] rows on bar_data to match point_data 
# this number corresponds to number of scatter points

add_rows <- tibble(
  status=rep("y1a_count",animated_sankey_master_list$n_scatter_coords-1),
  val=rep(0,animated_sankey_master_list$n_scatter_coords-1),
  destination=rep("Y2 Status A",animated_sankey_master_list$n_scatter_coords-1),
  frame=1:(animated_sankey_master_list$n_scatter_coords-1),
  x1=rep(0,animated_sankey_master_list$n_scatter_coords-1),
  x2=rep(0,animated_sankey_master_list$n_scatter_coords-1),
  max_y2=rep(0,animated_sankey_master_list$n_scatter_coords-1),
  y2=rep(0,animated_sankey_master_list$n_scatter_coords-1),
  color_col=rep("white",animated_sankey_master_list$n_scatter_coords-1)
)

bar_data <- bar_data %>% bind_rows(add_rows)

## get Ns and percentages for annotations

left_annotations <- sank_tib %>% 
  group_by(status) %>%
  summarize(n_count = n()) %>%
  mutate(freq = round((n_count/sum(n_count))*100,2))

right_annotations <- sank_tib %>%
  group_by(to) %>%
  summarize(n_count=n()) %>%
  mutate(freq = round((n_count/sum(n_count))*100,2)) %>%
  mutate(to = ifelse(to==animated_sankey_master_list$right_statuses_y$y2a_to,"Y2 Status A",
                     ifelse(to==animated_sankey_master_list$right_statuses_y$y2b_to,"Y2 Status B","Y2 Status C")))

## add buffers by status 

#...

### build polygon tib for left labels
y_adjust_val <- .37

label_rects <- tibble(
  
  xmin = c(animated_sankey_master_list$annotation_coords$left_annotations_x-.25),
  xmax = c(animated_sankey_master_list$annotation_coords$left_annotations_x+.25),
  ymin = c(
    animated_sankey_master_list$left_statuses_y$y1a-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1b-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1c-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1d-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1e-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1f-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1g-y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1h-y_adjust_val
  ),
  ymax = c(
    animated_sankey_master_list$left_statuses_y$y1a+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1b+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1c+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1d+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1e+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1f+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1g+y_adjust_val,
    animated_sankey_master_list$left_statuses_y$y1h+y_adjust_val
  ),
  color = c(
    "grey30",
    "grey35",
    "grey40",
    "grey45",
    "grey50",
    "grey55",
    "grey60",
    "grey65"
    )
)

##################### make plot and animation #########################

## make annotation data for geom_text

annotation_data <- tibble(
  
  x = c(
    # 
    rep(animated_sankey_master_list$annotation_coords$left_annotations_x,8),
    rep(animated_sankey_master_list$annotation_coords$right_annotations_x,3)
  ),
  y = c(
    # 
    animated_sankey_master_list$left_statuses_y$y1a,
    animated_sankey_master_list$left_statuses_y$y1b,
    animated_sankey_master_list$left_statuses_y$y1c,
    animated_sankey_master_list$left_statuses_y$y1d,
    animated_sankey_master_list$left_statuses_y$y1e,
    animated_sankey_master_list$left_statuses_y$y1f,
    animated_sankey_master_list$left_statuses_y$y1g,
    animated_sankey_master_list$left_statuses_y$y1h,
    animated_sankey_master_list$right_statuses_y$y2a_to,
    animated_sankey_master_list$right_statuses_y$y2b_to,
    animated_sankey_master_list$right_statuses_y$y2c_to
    
  ),
  label = c(
    # 
    paste0("Y1 Status A\nN = ",annotator("left","Y1 Status A")),
    paste0("Y1 Status B\nN = ",annotator("left","Y1 Status B")),
    paste0("Y1 Status C\nN = ",annotator("left","Y1 Status C")),
    paste0("Y1 Status D\nN = ",annotator("left","Y1 Status D")),
    paste0("Y1 Status E\nN = ",annotator("left","Y1 Status E")),
    paste0("Y1 Status F\nN = ",annotator("left","Y1 Status F")),
    paste0("Y1 Status G\nN = ",annotator("left","Y1 Status G")),
    paste0("Y1 Status H\nN = ",annotator("left","Y1 Status H")),
    paste0("Y2 Status A\nN = ",annotator("right","Y2 Status A")),
    paste0("Y2 Status B\nN = ",annotator("right","Y2 Status B")),
    paste0("Y2 Status C\nN = ",annotator("right","Y2 Status C"))
    
  ),
  family="Calibri",
  color = c(
    rep("white",8),
    c("mediumpurple4","mediumpurple3","mediumpurple2")
  )
)

### make empty right containers for stacked bars

empty_containers <- tibble(
  
  xmin = animated_sankey_master_list$right_statuses_x$x_min,
  xmax = animated_sankey_master_list$right_statuses_x$x_max,
  ymin = c(
  
    animated_sankey_master_list$bar_coord_list[[1]]$ymin, 
    animated_sankey_master_list$bar_coord_list[[2]]$ymin, 
    animated_sankey_master_list$bar_coord_list[[3]]$ymin 
    
  ),
  ymax = c(
    
    animated_sankey_master_list$bar_coord_list[[1]]$ymax, 
    animated_sankey_master_list$bar_coord_list[[2]]$ymax, 
    animated_sankey_master_list$bar_coord_list[[3]]$ymax  
    
  )
)

##### the plot #####
animp <- ggplot(point_data, aes(x,y)) +
    # scatter
    geom_point(
     shape=15,
     size=1,
     aes(color=status)
    ) +
    # bars
    geom_rect(
      data=bar_data,
      aes(xmin=x1,
      xmax=x2,
      ymin=max_y2,
      ymax=y2,
      alpha=0.5),
      fill=bar_data$color_col,
      inherit.aes=F) +
    # some general features
    scale_colour_manual(values = animated_sankey_master_list$plot_colors) + 
    theme_void() +
    coord_cartesian(xlim = c(-0.5,1.5)) + # 
    # left label boxes
    geom_rect(
      data=label_rects,
      aes(
        xmin=xmin,
        xmax=xmax,
        ymin=ymin,
        ymax=ymax,
        alpha=0.5),
      inherit.aes = F,
      fill=label_rects$color
    ) + 
    # right empty containers
    geom_rect(
      data=empty_containers,
      aes(xmin=xmin,
          xmax=xmax,
          ymin=ymin,
          ymax=ymax,
          alpha=0), # translucence
      color = "grey",
      inherit.aes = F
    ) + 
    # left labels
    geom_text(
      data=annotation_data,
      aes(x=x,y=y,label=label,family=family),
      color = annotation_data$color,
      inherit.aes = F
    ) + 
    # general plot features 
    theme(
      legend.position="none",
      text=element_text(size=17,family="Calibri"),
      plot.title = element_text(hjust = 0.52),
      plot.subtitle = element_text(hjust = 0.52)
      #plot.background = element_rect(fill='black')
    ) + 
    ggtitle(label = "Year 1 -> Year 2 Status Changes",
            subtitle = "(!this is all dummy data!)") +
    ### the below will convert this into an animation rather than static plot
    transition_time(frame) +
    enter_fade() +
    exit_fade() 

# render animation
animator <- animate(
  animp,
  nframes = 400,
  duration = 20,
  fps = 20
)

# save to static for review
# ggsave(
# 
#   filename = "static_anim.png",
#   plot = animp
# 
# )

#animator

# anim_save(
#   
#   filename = "sankey_animation.gif",
#   animation = animator
#   
# )

#TODO
#-add buffer in between statuses (if requested)
#-fix right bars
#--oft_to should be 1, 
#--and ideally the OT bars would top out at 8 to align with the left bars,
#--that would involve fiddling around with all bars y vals
#-scatter points are blobby. ideally fix those, but unwanted vertical breaks are the issue there
