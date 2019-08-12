# right bars - want them to span the entire y axis
#seq(from=1,to=8,length.out=6) 

animated_sankey_master_list <- list(
  n_scatter_coords = 200,
  left_statuses_y = list(
    y1a = 8,
    y1b = 7,
    y1c = 6,
    y1d = 5,
    y1e = 4,
    y1f = 3,
    y1g = 2,
    y1h = 1
  ),
  right_statuses_y = list(
    y2a_to = 6,
    y2b_to = 3.5,
    y2c_to = 0.9,
    y2a_y_start = 6.5,
    y2b_y_start = 4.1,
    y2c_y_start = 1.4
  ),
  right_statuses_x = list(
    x_min = 1.02,
    x_max = 1.5
  ),
  bar_coord_list =list( # seq(1.25,8,length.out=6)s
    list(
      destination_val="Y2 Status A",
      ymax=7.85,
      ymin=6.5
    ),
    list(
      destination_val="Y2 Status B",
      ymax=5.45,
      ymin=4.1
    ),
    list(
      destination_val="Y2 Status C",
      ymax=2.75,
      ymin=1.4
    )
  ),
  plot_colors = c(
    "Y1 Status A" = "grey30",
    "Y1 Status B" = "grey35",
    "Y1 Status C" = "grey40",
    "Y1 Status D" = "grey45",
    "Y1 Status E" = "grey50",
    "Y1 Status F" = "grey55",
    "Y1 Status G"="grey60",
    "Y1 Status H" = "grey65",
    "y1a_count" = "grey30",
    "y1b_count" = "grey35",
    "y1c_count" = "grey40",
    "y1d_count" = "grey45",
    "y1e_count" = "grey50",
    "y1f_count" = "grey55",
    "y1g_count"="grey60",
    "y1h_count" = "grey65",
    "Y2 Status A"="mediumpurple4",
    "Y2 Status B"="mediumpurple3",
    "Y2 Status C"="mediumpurple2"),
  annotation_coords = list(
    left_annotations_x = -0.25,
    right_annotations_x = 1.25
  )
)

sigmoid <- function(x_from, 
                    x_to, 
                    y_from, 
                    y_to, 
                    scale = 5, 
                    n = animated_sankey_master_list$n_scatter_coords) {
  
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tib <- tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
                y = y * (y_to - y_from) + y_from)
  return(tib)
  
}

generate_coords <- function(d_list,
                            n_groups){
  
  d_val <- d_list$destination_val
  y_max <- d_list$ymax
  y_min <- d_list$ymin
  
  total_length <- (
    
    n_persons_destination %>% filter(destination==d_val)
    
  )[["n_persons"]] 
  
  # generate the y2 vals to be plotted
  plot_seq <- seq(
    
    from = y_min,
    to = y_max,
    length.out = total_length+n_groups # corresponds to number of left groups
    
  )
  
  # need status numbers to assign coords
  
  n_status_destination <- bar_data %>% 
    group_by(destination,status) %>% 
    summarize(max_val=max(val)) %>% 
    filter(destination==d_val)
  
  n_status_list <- list(
    
    y1a_count=(n_status_destination
              %>%filter(status=='y1a_count'))[['max_val']],
    y1b_count=(n_status_destination%>%
               filter(status=='y1b_count'))[['max_val']],
    y1c_count=(n_status_destination
                 %>%filter(status=='y1c_count'))[['max_val']],
    y1d_count=(n_status_destination
              %>%filter(status=='y1d_count'))[['max_val']],
    y1e_count=(n_status_destination
                  %>%filter(status=='y1e_count'))[['max_val']],
    y1f_count=(n_status_destination%>%
               filter(status=='y1f_count'))[['max_val']],
    y1g_count=(n_status_destination%>%
                filter(status=='y1g_count'))[['max_val']],
    y1h_count=(n_status_destination
                    %>%filter(status=='y1h_count'))[['max_val']]
    
  )
  
  plot_tib <- tibble(y2 = plot_seq,
                     val = c(0:n_status_list[['y1a_count']],
                             0:n_status_list[['y1b_count']],
                             0:n_status_list[['y1c_count']],
                             0:n_status_list[['y1d_count']],
                             0:n_status_list[['y1e_count']],
                             0:n_status_list[['y1f_count']],
                             0:n_status_list[['y1g_count']],
                             0:n_status_list[['y1h_count']]
                     ),
                     status = c(rep('y1a_count',n_status_list[['y1a_count']]+1),
                                rep('y1b_count',n_status_list[['y1b_count']]+1),
                                rep('y1c_count',n_status_list[['y1c_count']]+1),
                                rep('y1d_count',n_status_list[['y1d_count']]+1),
                                rep('y1e_count',n_status_list[['y1e_count']]+1),
                                rep('y1f_count',n_status_list[['y1f_count']]+1),
                                rep('y1g_count',n_status_list[['y1g_count']]+1),
                                rep('y1h_count',n_status_list[['y1h_count']]+1)
                     ),
                     destination=d_val
  )
  
  return(plot_tib)
  
}

# this guy prepares the N+% strings for the annotations
annotator <- function(left_or_right,
                      sub_val){
  
  if (left_or_right=="left"){
    
    str1 <- (left_annotations %>% 
               filter(status==sub_val))[['n_count']]
    str2 <- paste0((left_annotations %>%
                      filter(status==sub_val))[['freq']],
                   "%")
    return_str <- paste0(str1," (",str2,")")
    
  } else {
    
    str1 <- (right_annotations %>% 
               filter(to==sub_val))[['n_count']]
    str2 <- paste0((right_annotations %>%
                      filter(to==sub_val))[['freq']],
                   "%")
    return_str <- paste0(str1," (",str2,")")
    
  }
  
  return(return_str)
  
}
