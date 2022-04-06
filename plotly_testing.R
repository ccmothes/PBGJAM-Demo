# test making species-env interaction plots

library(tidyverse)
library(plotly)

dat <- readRDS("PBGJAM-ShinyDemo/data/data4Caitlinv2.rds")
# list, 136 dataframes for each species


#plotly where choose scenario and main/full effect

#filter to species and scenario
sub <- dat$agonolConjun

sub %>% as_tibble() %>% 
plot_ly(x = ~s1.fullT, y = ~s1, marker = list(color = ~s1.fullT, colorscale = "Inferno", colorbar = list(title = 's1.fullT')),
        hovertemplate =  paste(.$plot.ID, "<extra></extra>"),
        type = "scatter", mode = "markers")

#try gganimate between s1 and s2

#ntransform data for scenario plotting
anim <- sub %>% pivot_longer(cols = contains("s")) %>% 
  mutate(scenario = if_else(str_detect(name, "s1"), "s1", "s2")) %>%
  mutate(name = case_when(name %in% c("s1", "s2") ~ "Abundance_change",
                          name %in% c("s1.fullT", "s2.fullT") ~ "Full_temp_effects",
                          name %in% c("s1.mainT", "s2.mainT") ~ "Main_temp_effects")) %>% 
  pivot_wider(names_from = name, values_from = value) %>% mutate(scenario = factor(scenario, levels = c("s1", "s2"))) %>% 
  mutate(paired = rep(1:(n()/2), each = 2)) %>% arrange(scenario)

# anim %>% 
#   plot_ly(x = ~gap.frac.10, y = ~Abundance_change, 
#           hovertemplate =  paste(.$plot.ID, "<extra></extra>"),
#           frame = ~scenario,
#           type = "scatter", mode = "markers") %>% 
#   layout(showlegend = FALSE) 
# 
# 
# anim %>% 
#   plot_ly(x = ~gap.frac.10, y = ~Abundance_change, size = ~Full_temp_effects,
#           hovertemplate =  paste(.$plot.ID, "<extra></extra>"),
#           frame = ~scenario,
#           type = "scatter", mode = "markers") %>% 
#   layout(showlegend = FALSE)  
# 
# #try again
# anim %>% 
#   plot_ly(x = ~gap.frac.10, y = ~Abundance_change, color = ~Full_temp_effects, frame = ~scenario,
#           hovertemplate =  paste(.$plot.ID, "<extra></extra>"), type = "scatter", mode = "markers") %>% 
#   layout(xaxis = list(type = "log"))


#can't get annimate to work with changing color, only size works
anim %>% 
  plot_ly(x = ~Full_temp_effects, y = ~Abundance_change, size = ~gap.frac.10,
          frame = ~scenario,
          #marker = list(color = ~gap.frac.10, colorbar = list(title = 'Summer Temp'), colorscale = "Portland"),
          hovertemplate =  paste(.$plot.ID, "<extra></extra>"),
          type = "scatter", mode = "markers") %>% 
  layout(showlegend = FALSE) 


# try gganimate
# library(gganimate)
# 
# 
# p <- ggplot(
#   anim, aes(x = gap.frac.10, y = Abundance_change, colour = Full_temp_effects))+
#   geom_point(alpha = 0.7)+
#   transition_states(scenario)
#takes wayy too long


# try arrows between scenarios

anim %>% 
ggplot(aes(x = Full_temp_effects, y = Abundance_change))+
  geom_path(aes(group = plot.ID), color = "darkgrey",
            arrow = arrow(length = unit(4, "mm"), ends = "last"), size = 0.75)+
  geom_point(aes(color = scenario), alpha = 0.7, size = 3) +
  theme_minimal()
 

# not working....
ggplotly(p) %>% 
  layout(annotations=list(
    list(
      x= anim$Full_temp_effects[1],
      y= anim$Abundance_change[1],
      showarrow=TRUE,
      #xref = "x",
      #yref = "y",
      arrowhead = 2,
      arrowsize = 10,
      arrowcolor = "black",
      ax=anim$Full_temp_effects[1],
      ay=anim$Abundance_change[1]
    )
  ))


# try plotly arrows

p <- anim[1:2,] %>% 
  plot_ly(x = ~Full_temp_effects, y = ~Abundance_change, color = ~scenario,
          #marker = list(color = ~gap.frac.10, colorbar = list(title = 'Summer Temp'), colorscale = "Portland"),
          hovertemplate =  paste(.$plot.ID, "<extra></extra>"),
          type = "scatter", mode = "markers") 


#try adding arrow for one pair
plot_ly(anim[1:2,]) %>% 
  add_markers(~Full_temp_effects, ~Abundance_change) %>% 
 add_annotations(
      x = anim[2,"Full_temp_effects"],
      y = anim[2, "Abundance_change"],
      ax = anim[1, "Full_temp_effects"],
      ay = anim[1, "Abundance_change"],
      xref = anim$Full_temp_effects,
      yref = anim$Abundance_change,
      axref = anim$Full_temp_effects,
      ayref = anim$Abundance_change,
      showarrow = TRUE,
      text = ""
    )

# try with og data
sub[1,]%>% 
  plot_ly() %>% 
  add_markers(x = ~s1.fullT, y = ~s1) %>% 
  add_markers(x = ~s2.fullT, y = ~s2) %>% 
  add_annotations(x = ~s2.fullT,
                  y = ~s2,
                  ax = ~s1.fullT,
                  ay = ~s1,
                  showarrow = TRUE,
                  text = "")


positiox_positiony <- data.frame(x_position=1:2,y_position=3:4)
node_edge_xy <- data.frame(x=1,y=3,xend=2,yend=4)
p <- ggplot(positiox_positiony,aes(x=x_position,y=y_position))+
  geom_point(color="red",size=10,alpha=0.8)+
  geom_segment(data=node_edge_xy,aes(x = x,y = y,xend = xend,yend = yend),
               arrow = arrow(length = unit(0.25,"cm"),ends="last",type="closed"))
p

ggplotly(p)

ggplotly(p) %>%
  layout(annotations=list(
    list(
      x=positiox_positiony$x_position[2],
      y=positiox_positiony$y_position[2],
      showarrow=TRUE,
      #xref = "x",
      #yref = "y",
      arrowhead = 2,
      arrowsize = 2,
      arrowcolor = "black",
      ax=-10,
      ay=10
    )
  ))



plot_ly(sub) %>% 
  add_annotations(x = ~s2.mainT,
                  y = ~s2,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  ax = ~s1.mainT, ay = ~s1,
                  text = "", showarrow = T, arrowcolor = "gray",
                  arrowwidth = 1,
                  opacity = 0.6, arrowsize = 2, arrowhead = 2) %>% 
  add_markers(~s1.mainT, ~s1) %>% 
  add_markers(~s2.mainT, ~s2, color = "red")

#try out with scenario formatted data
## reformat data, select x and y vars and then pivot longer

test <- noquote(paste0("Full_temp_effects", "_s1"))
anim %>%
  pivot_wider(names_from = scenario, values_from = c(Abundance_change, Full_temp_effects, Main_temp_effects)) %>%
  group_by(plot.ID) %>% 
  summarize(across(contains(c("s1","s2")), ~sum(.x, na.rm = TRUE))) %>% 
  plot_ly(hovertemplate =  paste("%{x},%{y}<br>","Site:", .$plot.ID, "<extra></extra>")) %>% 
  add_markers(x = as.formula(paste0("~", "Full_temp_effects", "_s1")), ~Abundance_change_s1, marker = list(color = "red", size = 8), name = "SSP245") %>% 
  add_markers(~Full_temp_effects_s2, ~Abundance_change_s2, marker = list(color = "darkred", size = 8), name = "SSP585") %>% 
  add_annotations(x = ~Full_temp_effects_s2,
                  y = ~Abundance_change_s2,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  ax = ~Full_temp_effects_s1, ay = ~Abundance_change_s1,
                  text = "", showarrow = T, arrowcolor = "gray",
                  arrowwidth = 1,
                  opacity = 0.6, arrowsize = 2, arrowhead = 5)




