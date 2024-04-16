##Packages===
library(tidyverse)
library(ggpubr)
library(ggcorrplot)
library(MuMIn)
library(colorspace)
library(DescTools)
library(viridis)
library(ggtext)
library(ggridges)
library(bstfun)
library(gt)
library(gtsummary)
library(patchwork)
###

data <- read_csv("data/data.csv")

dt <- ymd("2023/12/23")
new.dt <- dt - as.difftime(2, unit="days")

data <- data |> 
  mutate(`Date started` = lubridate::mdy(`Date started`)) |> 
  mutate(`Days of work` = as.numeric(dt - `Date started`)) |> 
  mutate(`Score-001` = replace_na(`Score-001`, 0)) |> 
  mutate(`October-time` = replace_na(`October-time`, 0)) |> 
  mutate(`Nov-time` = replace_na(`Nov-time`, 0)) |> 
  mutate(`learning-time` = `October-time` + `Nov-time`) |> 
  mutate(`Coursework-time` = replace_na(`Coursework-time`, 0)) |> 
  mutate(`Coursework-time` = `Coursework-time` - mean(`Coursework-time`, na.rm = T)) |> 
  mutate(`Commits` = replace_na(`Commits`, 0)) |> 
  mutate(`Commits` =  `Commits` - mean(`Commits`, na.rm = T)) |> 
  mutate(`learning-time` = `learning-time` - mean(`learning-time`, na.rm = T)) |> 
  mutate(`Coursework-time` = `Coursework-time` - mean(`Coursework-time`, na.rm = T)) |>
  mutate(`Days of work` = `Days of work` - mean(`Days of work`, na.rm = T)) |>  
  mutate(`Attendance` = 1-`In-person absence`) |> 
  mutate(`Attendance` = replace_na(`Attendance`, 0))


### Assignments

assignment_data <- read_csv("data/weekly-assignment.csv")


full_data <- full_join(data, assignment_data, by = "Username")

full_data <- full_data |> 
  rename("Wk1" = 16, "Wk2" = 17, "Wk3" = 18, "Wk4" = 19, "Wk6" = 20, "Wk7" = 21) |> 
  mutate(
    across(
      .cols = (16:21),
      .fns = ~(.x = if_else(is.na(.x), 0, 1))
    )) %>%
  mutate(weekly_assignments = rowSums(select(.data = . , Wk1:Wk6)))


full_data1 <- filter(full_data, `Score-001` > 0) |> 
  select(-c(`Username`,`Student ID.x`, `Student ID.y`, `Last Access`, `Availability`,  `Date started`, `October-time`, `Nov-time`, `In-person absence`)) |> 
  select(-c(Wk1:Wk7)) |> 
  drop_na()


# Figures====

# Learning time

pal1 <- c("#FF8C00", "#A034F0")

sub <- 'Average time on system in <span style ="color:#A034F0">October</span> and <span style ="color:#FF8C00">November</span> '

oct_nov <- data |> 
  select(c(`October-time`, `Nov-time`)) |> 
  rename("October" = `October-time`, "November" = `Nov-time`) |> 
  pivot_longer(cols = everything(), names_to = "month", values_to = "time") |> 
  ggplot(data = _, aes(x = `month`, y = `time`, fill = `month`, colour = `month`)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    aes(color = `month`,
        fill = after_scale(lighten(color, .5))),
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = .5, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA
  ) +
  ggdist::stat_dots(
    ## orientation to the left
    side = "left", 
    ## move geom to the left
    justification = 1.5, 
    ## adjust grouping (binning) of observations 
    binwidth = .25
  )+
  
  coord_flip(xlim = c(.9,1.8))+
  scale_color_manual(values = pal1, guide = "none")+
  scale_fill_manual(values = pal1, guide = "none")+
  labs(
    x = NULL,
    y = "Recorded hours",
    title = "",
    subtitle = paste(sub)
  ) +
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
      color = (darken(pal1, .1, space = "HLS")),
      size = 18
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot")


## Attendance

sub <- '25% of students attend more than <span style="color:#D6A607FF;">80% of classes</span> and 25% attend fewer than <span style="color:#481567FF;">40% of classes</span>.'

attendance_plot <- ggplot(data, aes(x = `Attendance`, y = 1, fill = factor(stat(quantile)))) +
  geom_density_ridges_gradient(calc_ecdf = TRUE, quantiles = 4, quantile_lines = TRUE,
                               alpha = .8) +
  scale_x_continuous(limits = c(0,1),
                     labels = scales::percent)+
  scale_fill_viridis_d(name = "Classroom Quartiles",
                       labels = c("<25%", "26-50%", "51-75%", ">75%")) +
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  labs(subtitle = paste(sub))+
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    axis.title.y = element_blank(),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot",
    legend.position = "bottom")
    



pal <- c(
  "gray25",
  rep("gray80", 4), 
  "#2A91A2"
)

sub <- '<span style ="color:#2A91A2"> The majority of students </span> completed every weekly task'



tasks_plot <- full_data |> 
  group_by(weekly_assignments) |> 
  summarise(count = n()) |> 
  mutate(weekly_assignments = as.factor(weekly_assignments)) |> 
  dplyr::mutate(perc = scales::percent(count / sum(count), accuracy = 1, trim = TRUE)) |> 
  
  ggplot(aes(x = weekly_assignments,
             y = count,
             fill = weekly_assignments))+
  geom_col()+
  geom_label(aes(label = perc),
             hjust = 1,
             fill = "white",
             fontface = "bold") +
  coord_flip()+
  scale_fill_manual(values = pal, guide = "none")+
  labs(
    x = NULL,
    y = "Number of students",
    title = "",
    subtitle = paste(sub)
  ) +
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
      color = (darken(pal, .1, space = "HLS")),
      size = 18
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot")


corr_plot <- full_data |> 
  select(-c(`Commits`,`Score-001`, `Username`,`Student ID.x`, `Student ID.y`, `Last Access`, `Availability`,  `Date started`, `October-time`, `Nov-time`, `In-person absence`)) |> 
  select(-c(Wk1:Wk7)) |> 
  rename("Coursework Days" = `Days of work`,
         "Coursework Hours" = `Coursework-time`,
         "Learning Hours" = `learning-time`,
         "Weekly Assignments" = `weekly_assignments`) |> 
  drop_na() |> 
  cor(x=_) |> 
  ggcorrplot(hc.order = TRUE, type = "lower",
             lab = TRUE)+
  labs(fill = "Correlation") +
  scale_fill_viridis(option = "cividis", limits = c(-.5,.6))+
  theme(legend.position = "bottom")


### Non submission

### Non-submission

full_data2 <- full_data|> 
  select(-c(`Username`,`Student ID.x`, `Student ID.y`, `Last Access`, `Availability`,
            `Coursework-time`, `In-person absence`, `learning-time`, `Commits`, `Date started`, `Days of work`)) |> 
  drop_na() |> 
  mutate(`Non-submission` = if_else(`Score-001` > 0, 0, 1)) |> 
  select(-`Score-001`)

load("model-bin")

risk_table <- tbl_regression(model_bin, exponentiate = T) |> as_ggplot()



# 8 scheduled contact hours



risk_plot <- emmeans::emmeans(model_bin, specs = ~`October-time` + `Attendance`, 
                              type = "response", 
                              at = list(`October-time` = c(1:6), 
                                        `Attendance` = seq(0,.8, by = .1))) |> 
  as_tibble() |> 
  mutate(submission = as.factor(if_else(prob > .3, 0, 1))) |> 
  mutate(perc = scales::percent(prob, accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(x = `October-time`, y = `Attendance`, fill = prob)) +
  geom_point(aes(size = prob),shape = 21, colour = "gray50", alpha = .7)+
  #  geom_tile(colour = "gray50", alpha = .7) +
  scale_fill_viridis_c(name = "Probability of non-submission")+
  geom_text(aes(label = perc, alpha = submission), colour = "gray10",size = 4) +
  scale_alpha_manual(values = c(1,0))+
  #   scale_fill_viridis(option = "magma") +  # Adjust colors as needed
  
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
     size = 18
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot")+scale_size_continuous(range  = c(2,15))+coord_cartesian(ylim = c(-.05, .8))+
  guides(size = "none", alpha = "none")+
  labs(y = "In-person Attendance",
       x= "Learning time in hours (month of October)")
  



risk_plot + inset_element(risk_table, left = 0.61, bottom = 0.6, right = 1, top = 1.1) 

