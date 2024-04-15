library(tidyverse)
library(ggtext)
theme_set(theme_minimal())

scores <- tibble(
  #part = c("a", "b", "c", "d"),
  part = paste("Task", c(1:4)),
  `written` = c(286, 254, 270, 228),
  `e-assessment` = c(177, 139, 166, 113)
) %>% 
  pivot_longer(cols = -part, names_to = "format", values_to = "num_correct") %>% 
  mutate(num_responses = ifelse(format == "written", 333, 322)) %>% 
  mutate(prop_correct = num_correct / num_responses)

scores %>%
  ggplot(aes(
    x = part,
    y = prop_correct,
    colour = format,
    group = format
  )) +
  geom_point() +
  geom_text(aes(label = str_glue("{num_correct}")), position = position_nudge(y = 0.025)) +
  geom_text(
    data = scores %>% filter(part == "Task 4"),
    aes(
      label = str_glue("{format}\n(N={num_responses})"),
      x = part,
      y = prop_correct,
      color = format
    ),
    hjust = 0,
    position = position_nudge(x = 0.2)
  ) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  expand_limits(y = c(0, 1), x = 5) +
  labs(x = "", y = "Proportion correct") +
  guides(colour = FALSE)

scores %>%
  ggplot(aes(
    x = part,
    y = num_correct,
    group = format
  )) +
  geom_point() +
  geom_text(
    aes(label = str_glue("{round(prop_correct, 2)*100}%")),
    nudge_y = 23
  ) +
  geom_richtext(
    data = scores %>% filter(part == "Task 4"),
    aes(
      label = str_glue("<b>{format}</b><br>(N={num_responses})"),
      x = part,
      y = num_correct,
    ),
    hjust = 0,
    position = position_nudge(x = 0.5),
    fill = NA, label.color = NA
  ) +
  geom_line() +
  expand_limits(y = c(0, 1), x = 6.5) +
  labs(x = "", y = "Correct responses")
ggsave("paper/FIG_correctness.pdf", units = "cm", width = 10, height = 7)
ggsave("paper/FIG_correctness.svg", units = "cm", width = 10, height = 7)
