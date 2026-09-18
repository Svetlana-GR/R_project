# Загружаем и актуализируем нужные библиотеки+Блокировка предупреждений о конфликтующих функциях
options(warn = -1)
suppressPackageStartupMessages({
  library(tidyverse) # dplyr, ggplot2, readr и др.
  library(janitor)   # Для clean_names()
  library(ggsci)     # Палитры pal_npg()
  library(scales)    # Для красивых подписей осей
})

#  Проверка наличия директорий 
if (!dir.exists("data")) stop("Папка data не найдена")
if (!dir.exists("output")) dir.create("output")

#  Этап 1. Загрузка данных 
raw_data <- read_csv2("data/data_raw.csv")
clean_data <- raw_data %>% janitor::clean_names()

#  Этап 2. Препроцессинг
full_prep_data <- clean_data %>%
  filter(
    education_level == "Higher Education",
    degree %in% c("Bachelor's degree", "Master's degree", "Specialist's degree")
  ) %>%
  
  # Создаем метку уровня обучения для графиков
  mutate(
    study_level = case_when(
      degree == "Bachelor's degree" ~ "Бакалавриат",
      degree == "Master's degree" ~ "Магистратура",
      degree == "Specialist's degree" ~ "Специалитет"
    ),
    
    broad_field = case_when(
      branches_of_science %in% c("Mathematical and Natural Sciences", "Engineering and Technology") ~ "STEM",
      branches_of_science == "Humanities" ~ "Гуманитарные науки",
      branches_of_science == "Arts and Culture" ~ "Искусство и культура",
      TRUE ~ "Прочее"
    )
  ) %>%
  
  group_by(year, study_level, broad_field) %>%
  summarise(total_applications = sum(number_of_applications, na.rm = TRUE), .groups = "drop")


#  Этап 3. Статистические тесты (для каждого уровня образования отдельно)
cat("\n--- СТАТИСТИЧЕСКИЕ ТЕСТЫ: STEM vs Гуманитарные науки ---\n")

for (level in unique(full_prep_data$study_level)) {
  cat("\nУровень:", level, "\n")
  
  temp_df <- full_prep_data %>% filter(study_level == level & broad_field %in% c("STEM", "Гуманитарные науки"))
  
  stem_vals <- temp_df %>% filter(broad_field == "STEM") %>% pull(total_applications)
  human_vals <- temp_df %>% filter(broad_field == "Гуманитарные науки") %>% pull(total_applications)
  
  mw_test <- wilcox.test(stem_vals, human_vals, alternative = "two.sided", exact = FALSE)
  r_value <- as.numeric(mw_test$statistic) / (length(stem_vals) * length(human_vals))
  
  print(mw_test)
  cat("Размер эффекта r:", round(r_value, 3), "\n")
}

#  Этап 4. Визуализация 
# График 1: Динамика поданныз заявлений(на примере бакалавриата)
pub_plot <- ggplot(prep_data, aes(x = year, y = total_applications, color = broad_field)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(
    name = "Направление",
    values = pal_npg()(4),
    labels = c("STEM" = "Инженерия и естественные науки", 
               "Гуманитарные науки" = "Гуманитарные науки", 
               "Искусство и культура" = "Искусство и культура", 
               "Прочее" = "Прочие")
  ) +
  
  labs(
    title = "Динамика числа заявлений в российские вузы (бакалавриат)",
    subtitle = "Разрыв между техническим и гуманитарным контурами. Данные 2014–2023 гг.",
    x = "Год",
    y = "Число заявлений (логарифмическая шкала)",
    caption = "Источник: датасет набора 2014–2023 | Расчеты автора"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(pub_plot)

ggsave(filename = "output/applications_trend.png", plot = pub_plot, width = 10, height = 6, dpi = 300)


## График2: Сравнительный тренд уровней 
pub_plot_levels <- ggplot(full_prep_data %>% filter(broad_field == "STEM"), 
                          aes(x = year, y = total_applications, color = study_level)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(
    name = "Уровень образования",
    values = pal_startrek("uniform")(3) 
  ) +
  
  labs(
    title = "Борьба за инженеров: Бакалавриат против Магистратуры",
    subtitle = "Динамика заявлений только на технические направления (STEM). Масштаб разрыва.",
    x = "Год",
    y = "Число заявлений (логарифмическая шкала)",
    caption = "Источник: датасет набора 2014–2023 | Расчеты автора"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("output/levels_comparison_stem.png", plot = pub_plot_levels, width = 10, height = 6, dpi = 300)
print(pub_plot_levels)

## График 3: Детальный срез одного года(2023г.)
final_year <- max(full_prep_data$year, na.rm = TRUE)
bar_data <- full_prep_data %>% 
  filter(year == final_year, broad_field != "Прочее") %>%
  pivot_wider(names_from = broad_field, values_from = total_applications, values_fill = 0)

pub_plot_bar <- ggplot(bar_data, aes(x = reorder(study_level, -STEM))) + 
  geom_col(aes(y = STEM, fill = "STEM"), width = 0.6) +
  geom_col(aes(y = `Гуманитарные науки`, fill = "Гуманитарные науки"), width = 0.4) +
  geom_col(aes(y = `Искусство и культура`, fill = "Искусство и культура"), width = 0.2) +
  
  scale_fill_brewer(palette = "Dark2", name = "Направление") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  
  coord_flip() +
  labs(
    title = paste("Структура приема в", final_year, "году"),
    subtitle = "Распределение заявок по направлениям внутри разных ступеней высшего образования.",
    x = "Уровень образования",
    y = "Число поданных заявлений",
    caption = "Источник: датасет набора 2014–2023 | Расчеты автора"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggsave("output/bar_structure_final_year.png", plot = pub_plot_bar, width = 8, height = 4, dpi = 300)
print(pub_plot_bar)    
