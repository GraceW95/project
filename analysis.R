## @knitr analysis-setup
library(ggplot2)
library(magrittr) 
individual <- readr::read_csv(here::here("data", "individual.csv")) %>% 
  dplyr::select(stem_diameter, height, growth_form)

## @knitr analysis-filter-data
analysis_df <- individual %>% 
  filter(!is.na(growth_form), growth_form != "liana")
gf_levels <- table(analysis_df$growth_form) %>% 
  sort() %>% 
  names()
analysis_df %<>% 
  mutate(growth_form = factor(growth_form, 
                              levels = gf_levels))

## @knitr analysis-fig1-barplot
analysis_df %>% 
  ggplot(aes(y = growth_form, colour = growth_form, fill = growth_form))+
  geom_bar(alpha = 0.5, show.legend = FALSE)

## @knitr analysis-fig2-facet-grid 
analysis_df %>% 
  tidyr::pivot_longer(col = c(stem_diameter, height),
                      values_to = "value",
                      names_to = "var") %>% 
  ggplot(aes(x = log(value), y = growth_form, colour = growth_form, 
             fill = growth_form)) +
  geom_violin(alpha = 0.5, trim = T, show.legend = FALSE) +
  geom_boxplot(alpha = 0.7, show.legend = F) +
  facet_grid(~var)

## @knitr analysis-lm-overall
lm_overall <- lm(log(stem_diameter) ~ log(height), analysis_df)
lm_overall %>% 
  broom::glance()
lm_overall %>% 
  broom::tidy()

## @knitr analysis-lm-fig3-overall
analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stem_diameter))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm)

## @knitr analysis-lm-fig4-growth

lm_growth <- lm(log(stem_diameter) ~ log(height) * growth_form, analysis_df)
lm_growth %>% 
  broom::glance()
lm_growth %>%
  broom::tidy()
analysis_df %>% 
  ggplot(aes(x = log(height), y = log(stem_diameter), colour = growth_form)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = lm)

