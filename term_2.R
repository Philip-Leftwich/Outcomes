### Final results
library(GGally)
library(betareg)
data2 <- read_csv("data/final_results.csv")


data2 |> select(-c(Username, `Student ID`, `Last Access`, Availability)) |> 
  filter(! `Final Assignment` %in% c("In Progress", "Needs Marking(69.00)")) |> 
  mutate(`Final Assignment` = as.numeric(`Final Assignment`)) |> 
  mutate(across(.cols = where(is.numeric),.fns = ~replace_na(.x, 0))) |> 
  ggpairs()


data3 <- readxl::read_xlsx("data/term_2_hours.xlsx")

combined <- full_join(data2, data3) |> drop_na(Hours)

combined |> select(-c(`Username`, `Last Access`, `Availability`, `Student ID`)) |>  #select(`Final Assignment`, `Weekly tests`, `Course test`, `October-time`, `Nov-time`, `Attendance`, `learning-time`) |> 
  filter(! `Final Assignment` %in% c("In Progress", "Needs Marking(69.00)")) |> 
  mutate(`Final Assignment` = as.numeric(`Final Assignment`)) |> 
  mutate(across(.cols = where(is.numeric),.fns = ~replace_na(.x, 0))) |> 
  ggpairs()

combined_2 <- combined |> select(-c(`Username`, `Last Access`, `Availability`, `Student ID`)) |>
  filter(! `Final Assignment` %in% c("In Progress", "Needs Marking(69.00)")) |> 
  mutate(`Final Assignment` = as.numeric(`Final Assignment`)) |> 
  mutate(across(.cols = where(is.numeric),.fns = ~replace_na(.x, 0)))

combined_2a <- combined_2 |> filter(`Final Assignment` > 0) |> mutate(`Final Assignment` = `Final Assignment`/100)

combined_3 <- combined_2 |>  
  mutate(`Non-submission` = if_else(`Final Assignment` > 0, 0, 1)) |> select(-`Final Assignment`)




full_model_bin <- glm(`Non-submission` ~ Hours, data = combined_3, family = binomial, na.action = "na.fail")

sub <- 'Everyone engaged in <span style ="color:#2A91A2"> at least 5 hours of work in February </span> submitted an assignment in June'

plot_1 <- emmeans::emmeans(full_model_bin, specs = ~ Hours, at = list(`Hours` = seq(0,20, by = 1)), type = "response") |> 
  as_tibble() |> 
  ggplot(aes(x = Hours,
             y = 1-prob))+
  geom_line()+
  geom_ribbon(aes(ymin=1-asymp.LCL, ymax=1-asymp.UCL), alpha = 0.3,
              fill ="#2A91A2") +
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
    plot.title.position = "plot")+
  guides(size = "none", alpha = "none")+
  labs(y = "Probability of submission",
       x= "Learning time in hours (January)",
       subtitle = paste(sub))+
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  geom_jitter(data = combined_3, aes(x = `Hours`, y = abs(1-`Non-submission`)), width = 1, height = 0,
              shape = 21, fill = "white", colour = "#2A91A2", size = 1.4)+
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
    plot.title.position = "plot")+
  guides(size = "none", alpha = "none")+
  labs(y = "Probability of submission",
       x= "Learning time in hours (January)",
       subtitle = paste(sub))






full_model <- betareg(`Final Assignment` ~ `Course test` + Hours, data = combined_2a, na.action = "na.fail")

full_table <- tbl_regression(full_model) |> as_ggplot()

best <- dredge(full_model)

best_model <- get.models(best, subset = 1)[[1]]     


sub2 <- '<span style ="color:#2A91A2"> Early engagment predicts coursework attainment </span>'

plot_2 <- emmeans::emmeans(full_model, specs = ~ Hours, at = list(`Hours` = seq(0,25, by = 1)), type = "response") |> 
  as_tibble() |> 
  ggplot(aes(x = Hours,
             y = emmean))+
  geom_line()+
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha = 0.3,
              fill ="#2A91A2") +
  scale_y_continuous(labels = scales::percent, limits = c(.3,1))+ 
  geom_point(data = combined_2a |> filter(Hours > 0), aes(x = `Hours`, y = `Final Assignment`),
             shape = 21, fill = "white", colour = "#2A91A2", size = 1.4)+
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
    plot.title.position = "plot")+
  guides(size = "none", alpha = "none")+
  labs(y = "Coursework scores",
       x= "Learning time in hours (January)",
       subtitle = paste(sub2))

plot_2 + inset_element(full_table, left = 0.61, bottom = 0, right = 1, top = .4) 
