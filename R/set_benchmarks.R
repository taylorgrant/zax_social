# Zaxby's Organic Benchmarking # 

pacman::p_load(tidyverse, janitor, here, glue)

df <- read_csv(here("data", "zaxby_post_1.1.23-5.31.24.csv")) |> 
  clean_names() |> 
  filter(!is.na(engagements)) |> # filter out empty engagements (1 post)
  filter(!network %in% c("LinkedIn", "YouTube")) |> 
  filter(post_type != "'@Reply") |> 
  mutate(paid_impressions = ifelse(is.na(paid_impressions), 0, paid_impressions),
         network = case_when(network == "Instagram" & post_type == "Story" ~ "Instagram Story",
                             network == "Instagram" & post_type %in% c("Post", "Reel") ~ "Instagram Post",
                             TRUE ~ network)) |> 
  filter(paid_impressions == 0) |> 
  filter(!(network == "X" & post_type == "Quote")) |> 
  filter(post_type != "Ad Post") |> 
  filter(impressions != 0) |> 
  mutate(date2 = as.Date(date, format = "%m/%d/%Y")) |> 
  arrange(date2)


# BENCHMARKS --------------------------------------------------------------

# there are some outliers for each platform that I want to drop
p <- c(.05, .7, .95)
p_names <- map_chr(p, ~paste0('p_',.x*100))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

# now get the thresholds (i want to drop any values at or greater)
# REACH # 
thresh_imp <- df |> 
  mutate(imp = case_when(is.na(organic_impressions) ~ impressions,
                         organic_impressions < impressions ~ organic_impressions,
                         TRUE ~ organic_impressions)) |> 
  mutate(imp = case_when(network == "TikTok" ~ video_views,
                         TRUE ~ imp)) |> 
  # filter(organic_impressions != 0) |> 
  group_by(network) |> 
  summarise(across(imp, tibble::lst(!!!p_funs))) 

# ENGAGEMENT RATE #
thresh_er <- df |> 
  mutate(imp = case_when(is.na(organic_impressions) ~ impressions,
                         organic_impressions < impressions ~ organic_impressions,
                         TRUE ~ organic_impressions)) |> 
  filter(imp > 0) |> 
  mutate(engagement_rate = engagements/imp) |> 
  group_by(network) |> 
  summarise(across(engagement_rate, tibble::lst(!!!p_funs)), 
            n = n()) |> 
  data.frame()

thresh_er |> mutate(across(contains("engagement"), ~scales::percent(., accuracy = .01)))
# ACTUAL BENCHMARKS -------------------------------------------------------

# reach benchmarks (dropping reach >= .95 percentile)
bm_imp <- df |> 
  mutate(imp = case_when(is.na(organic_impressions) ~ impressions,
                         organic_impressions < impressions ~ organic_impressions,
                         TRUE ~ organic_impressions)) |> 
  mutate(imp = case_when(network == "TikTok" ~ video_views,
                         TRUE ~ imp)) |> 
  mutate(drop = case_when(network == "Facebook" & imp >= 121364 ~ 1,
                          network == "Instagram Post" & imp >= 103053 ~ 1,
                          network == "Instagram Story" & imp >= 4037 ~ 1,
                          network == "TikTok" & imp >= 477693 ~ 1,
                          network == "X" & imp >= 36484 ~ 1,
                          TRUE ~ 0)) |>
  filter(drop != 1) |>
  group_by(network) |> 
  summarise(bm_imp = mean(imp, na.rm = TRUE),
            n = n())


# engagement rate (dropping outliers -> ER > .95 percentile)
bm_er <- df |> 
  mutate(imp = case_when(is.na(organic_impressions) ~ impressions,
                         organic_impressions < impressions ~ organic_impressions,
                         TRUE ~ organic_impressions)) |> 
  filter(imp > 0) |> 
  mutate(engagement_rate = engagements/imp) |> 
  mutate(drop = case_when(network == "Facebook" & engagement_rate >= 0.09337062 ~ 1,
                          network == "Instagram Post" & engagement_rate >= 0.104231127 ~ 1,
                          network == "Instagram Story" & engagement_rate >= 0.008310806 ~ 1,
                          network == "TikTok" & engagement_rate >= 0.12811256 ~ 1,
                          network == "X" & engagement_rate >= 0.040795396 ~ 1,
                          TRUE ~ 0)) |>
  filter(drop != 1) |>
  group_by(network) |> 
  summarise(bm_er = weighted.mean(engagement_rate, imp), n = n())

saveRDS(
  list(bm_er = bm_er, bm_imp = bm_imp),
  here('data', 'benchmarks.rds')
)
