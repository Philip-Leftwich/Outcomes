##Packages===
library(tidyverse)
library(ggpubr)
library(ggcorrplot)
library(MuMIn)
library(colorspace)
library(DescTools)
library(viridis)
library(bstfun)
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

  

data1 <- filter(data, `Score-001` >0)

model <- lm(`Score-001` ~ `learning-time` + `Coursework-time` + `Commits` + `Attendance` + `Days of work`, data = data1)

#importance = varImp(model, scale=FALSE)

data2 <- data |> 
  mutate(`Non-submission` = if_else(`Score-001` > 0, 0, 1))

model_bin <- glm(`Non-submission` ~ `October-time` + `Attendance`, data = data2, family = binomial)





###

pal <- c("#FF8C00", "#A034F0")

data |> 
  select(c(`October-time`, `Nov-time`)) |> 
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
  ) +
  stat_summary(
    geom = "text",
    fun = "median",
    aes(label = paste(round(..y.., 2), "hours"),
        color = month,
        color = after_scale(darken(color, .1, space = "HLS"))),
    family = "Roboto Mono",
    fontface = "bold",
    size = 4,
    vjust = 6
  )+
  
  coord_flip(xlim = c(.9,1.8))+
  scale_color_manual(values = pal, guide = "none")+
  scale_fill_manual(values = pal, guide = "none")+
  theme_pubr(base_family = "Zilla Slab", base_size = 15) +
  labs(x = "Time (hrs)",
       y = "")





### Simple figures

data |> 
  ggplot(aes(x = `Coursework-time`,
             y = `Attendance`)) +
  geom_point()+
  stat_cor(aes(label = ..r.label..))+
  theme_pubr()+
  scale_x_continuous(limits = c(-20,20))

data1 |>
  select(`Days of work`, Commits, `Coursework-time`,`learning-time`, `Attendance`) |> 
  drop_na() |> 
  cor(x=_) |> 
  ggcorrplot(hc.order = TRUE, type = "upper",
             lab = TRUE)+
  scale_fill_viridis(option = "cividis", limits = c(-.5,.5))
  

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
  select(-c(`Username`,`Student ID.x`, `Student ID.y`, `Last Access`, `Availability`,  `Date started`, `October-time`, `Nov-time`, `In-person absence`, `Commits`)) |> 
  select(-c(Wk1:Wk7)) |> 
  drop_na()

full_model <- lm(`Score-001` ~ (.)^2, data = full_data1, na.action = "na.fail")

best <- dredge(full_model)

#best_model <- get.models(best, subset = 1)[[1]]

summary(model.avg(best, subset = delta < 1)) |> 
  tbl_regression()


full_data1 |> 
select(-c(`Score-001`)) |> 
  drop_na() |> 
  cor(x=_) |> 
  ggcorrplot(hc.order = TRUE, type = "upper",
             lab = TRUE)+
  scale_fill_viridis(option = "cividis", limits = c(-.5,.5))
  

### Non-submission

full_data2 <- full_data|> 
  select(-c(`Username`,`Student ID.x`, `Student ID.y`, `Last Access`, `Availability`,
            `Coursework-time`, `In-person absence`, `learning-time`, `Commits`, `Date started`, `Days of work`)) |> 
  drop_na() |> 
  mutate(`Non-submission` = if_else(`Score-001` > 0, 0, 1)) |> 
  select(-`Score-001`)

full_model_bin <- glm(`Non-submission` ~ ., data = full_data2, family = binomial, na.action = "na.fail")

bin_best <- dredge(full_model_bin)

best_model_bin <- get.models(bin_best, subset = 1)[[1]]

model_bin <- glm(`Non-submission` ~ `October-time` + Attendance, family = binomial, data = full_data2)


risk_table <- tbl_regression(best_model_bin, exponentiate = T) |> as_ggplot()



# 8 scheduled contact hours

risk_plot <- emmeans::emmeans(best_model_bin, specs = ~`October-time` + `Attendance`, 
                 type = "response", 
                 at = list(`October-time` = c(1:6), 
                           `Attendance` = seq(0,.8, by = .1))) |> 
  as_tibble() |> 
  mutate(submission = as.factor(if_else(prob > .3, 0, 1))) |> 
  mutate(perc = scales::percent(prob, accuracy = 1, trim = FALSE)) |> 
  ggplot(aes(x = `October-time`, y = `Attendance`, fill = prob)) +
  geom_point(aes(size = prob),shape = 21, colour = "gray50", alpha = .7)+
#  geom_tile(colour = "gray50", alpha = .7) +
  scale_fill_viridis_c()+
  geom_text(aes(label = perc, alpha = submission), colour = "gray10",size = 4) +
  scale_alpha_manual(values = c(1,0))+
  #   scale_fill_viridis(option = "magma") +  # Adjust colors as needed
  theme_classic()+scale_size_continuous(range  = c(2,15))+coord_cartesian(ylim = c(-.05, .8))+
  guides(size = "none", alpha = "none")+
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono", size = 14),
    axis.text.y = element_text(family = "Roboto Mono",
      size = 14
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    axis.title.y = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21))




risk_plot + inset_element(risk_table, left = 0.61, bottom = 0.6, right = 1, top = 1.1) +
  plot_annotation(title = "Risk of non-submission") 
 
### Attendance

data |> 
  ggplot(data = _, aes(x = `Attendance`)) + 
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
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
  ) +
  stat_summary(
    geom = "text",
    fun = "median",
    aes(label = paste(round(..y.., 2), "hours")),
    family = "Roboto Mono",
    fontface = "bold",
    size = 4,
    vjust = 6
  )+
  
  coord_flip(xlim = c(.9,1.8))+
  scale_color_manual(values = pal, guide = "none")+
  scale_fill_manual(values = pal, guide = "none")+
  theme_pubr(base_family = "Zilla Slab", base_size = 15) +
  labs(x = "Time (hrs)",
       y = "")


pal <- c(
  "gray85",
  rep("gray70", 4), 
  "goldenrod1"
)

sub <- 'Reaction times for <span style ="color:#FF8C00">Sedentary</span>, <span style = "color:#A034F0">Moderately Active</span> and <span style = "color:#159090">Very active</span> students'



plot <- full_data |> 
  group_by(weekly_assignments) |> 
  summarise(count = n()) |> 
  mutate(weekly_assignments = as.factor(weekly_assignments)) |> 
  dplyr::mutate(perc = scales::percent(count / sum(count), accuracy = 1, trim = TRUE)) |> 

  ggplot(aes(x = weekly_assignments,
             y = count,
             fill = weekly_assignments))+
  geom_col()+
  geom_label(aes(label = perc),
            hjust = 1.1,
            fill = "white",
            fontface = "bold") +
  coord_flip()+
  scale_fill_manual(values = pal, guide = "none")+
  labs(
    x = NULL,
    y = "Reaction Time in milliseconds (ms)",
    title = "",
    subtitle = paste(sub),
    caption = "https://humanbenchmark.com/tests/reactiontime"
  ) +
  theme_minimal(base_family = "Zilla Slab", base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_text(family = "Roboto Mono"),
    axis.text.y = element_text(
      color = rev(darken(pal, .1, space = "HLS")),
      size = 18
    ),
    axis.title.x = element_text(margin = margin(t = 10),
                                size = 16),
    plot.title = element_markdown(face = "bold", size = 21),
    plot.subtitle = element_markdown(),
    plot.title.position = "plot")


