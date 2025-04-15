# monthly performance data for zax
monthly_performance <- function(current_month){
  
  pacman::p_load(tidyverse, janitor, here, glue, googlesheets4)
  
  # thresholds for bucketing 
  thresholds <- tibble(network = c("Facebook", "Instagram Post", "Instagram Story",
                                   "TikTok", "X"),
                       imp_p_5 = c(12100, 11700, 1800, 1200, 3400),
                       imp_p_70 = c(54500, 26800, 2400, 31500, 9800),
                       imp_p_95 = c(121000, 103000, 4000, 477000, 36500),
                       er_p_5 = c(.0166, .0434, .0003, .0283, .0180),
                       er_p_70 = c(.033, .0705, .0014, .0535, .0268),
                       er_p_95 = c(.093, .104, .0083, .128, .0408))
  
  # organic impression fn
  get_organic_impressions <- function(tbl){
    tbl |> 
      mutate(imp = case_when(is.na(organic_impressions) ~ impressions,
                             organic_impressions < impressions ~ organic_impressions,
                             TRUE ~ organic_impressions))
  }
  
  
  # set up previous month
  # set up current and previous month/year
  previous_month <- ((current_month - 2) %% 12) + 1
  previous_year <- lubridate::year(Sys.Date()) - ifelse(current_month %in% c(12,1), 1, 0)
  current_year <- lubridate::year(Sys.Date()) - ifelse(current_month %in% c(12), 1, 0)
  
  previous_data <- glue::glue(here::here("data", "zaxby_post_{previous_month}.{previous_year}.csv"))
  current_data <- glue::glue(here::here("data", "zaxby_post_{current_month}.{current_year}.csv"))
  
  if (!file.exists(current_data)) stop(glue("Missing current data file: {current_data}"))
  if (!file.exists(previous_data)) stop(glue("Missing previous data file: {previous_data}"))
  
  # 1. READ IN DATA ----------------------------------------------------------
  previous <- read_csv(previous_data) |> 
    clean_names() |> 
    filter(!is.na(engagements)) |> # filter out empty engagements (1 post)
    filter(!network %in% c("LinkedIn", "YouTube", "Threads")) |> 
    filter(post_type != "'@Reply") |> 
    mutate(paid_impressions = ifelse(is.na(paid_impressions), 0, paid_impressions),
           network = case_when(network == "Instagram" & post_type == "Story" ~ "Instagram Story",
                               network == "Instagram" & post_type %in% c("Post", "Reel") ~ "Instagram Post",
                               TRUE ~ network)) |> 
    filter(!(network == "X" & post_type == "Quote")) |> 
    filter(paid_impressions == 0) |>
    filter(impressions > 100) |> # to knock out random posts
    mutate(date2 = as.Date(date, format = "%m/%d/%Y")) |> 
    arrange(date2)
  
  current <- read_csv(current_data) |> 
    clean_names() |> 
    filter(!is.na(engagements)) |> # filter out empty engagements (1 post)
    filter(!network %in% c("LinkedIn", "YouTube", "Threads")) |> 
    filter(post_type != "'@Reply") |> 
    mutate(paid_impressions = ifelse(is.na(paid_impressions), 0, paid_impressions),
           network = case_when(network == "Instagram" & post_type == "Story" ~ "Instagram Story",
                               network == "Instagram" & post_type %in% c("Post", "Reel") ~ "Instagram Post",
                               TRUE ~ network)) |> 
    filter(!(network == "X" & post_type == "Quote")) |> 
    filter(paid_impressions == 0) |>
    filter(impressions > 100) |> # to knock out random posts
    mutate(date2 = as.Date(date, format = "%m/%d/%Y")) |> 
    arrange(date2) |> 
    mutate(month = floor_date(date2, "month")) |> 
    select(-date2)
  
  # read in benchmarks per platform
  benchmarks <- readRDS(here::here("data", "benchmarks.rds"))
  
  # 2. CALCULATE MONTHLY TOTALS ------------------------------------------------
  previous_totals <- previous |> 
    get_organic_impressions() |>  
    mutate(imp = case_when(network == "TikTok" ~ video_views,
                           TRUE ~ imp),
           month = floor_date(as.Date(date, format = "%m/%d/%Y"), "month")) |> 
    summarise(month = unique(month),
              post_count = n(),
              impressions = sum(imp), 
              engagements = sum(engagements),
              er = sum(engagements)/impressions)
  
  current_totals <- current |> 
    get_organic_impressions() |> 
    mutate(imp = case_when(network == "TikTok" ~ video_views,
                           TRUE ~ imp),
           month = floor_date(as.Date(date, format = "%m/%d/%Y"), "month")) |> 
    summarise(month = unique(month),
              post_count = n(),
              impressions = sum(imp), 
              engagements = sum(engagements),
              er = sum(engagements)/impressions) 
  
  monthly_totals <- rbind(previous_totals, current_totals) |> 
    mutate(network = 'Overall', 
           across(impressions:er, ~(. - lag(.))/lag(.),
                  .names = "{.col}_MoM")) |> 
    filter(month == current_totals$month) |>
    mutate(across(matches("ER|MOM"), ~round(., digits = 3))) |> 
    relocate(network, .before = month) %>%
    set_names(nm = toupper(names(.))) 
  
  # 3. PLATFORM PERFORMANCE ----------------------------------------------------
  previous_platform_performance <- previous |> 
    get_organic_impressions() |> 
    mutate(imp = case_when(network == "TikTok" ~ video_views,
                           TRUE ~ imp),
           month = floor_date(as.Date(date, format = "%m/%d/%Y"), "month")) |> 
    group_by(network) |> 
    summarise(prev_impressions = mean(imp, na.rm = TRUE),
              prev_engagements = sum(engagements),
              prev_er = sum(engagements)/sum(imp),
              prev_n = n())
  
  platform_performance <- current |> 
    get_organic_impressions() |> 
    mutate(imp = case_when(network == "TikTok" ~ video_views,
                           TRUE ~ imp),
           month = floor_date(as.Date(date, format = "%m/%d/%Y"), "month")) |> 
    group_by(network, month) |> 
    summarise(impressions = sum(impressions),
              avg_impressions = round(mean(imp, na.rm = TRUE)),
              engagements = sum(engagements),
              er = round(sum(engagements)/sum(imp), 3),
              post_count = n()) |> 
    left_join(previous_platform_performance) |> 
    left_join(select(benchmarks$bm_imp, -n), by = "network") |> 
    left_join(select(benchmarks$bm_er, -n), by = "network") |> 
    mutate(mom_imp_delta = round((avg_impressions - prev_impressions)/prev_impressions, 3),
           mom_er_delta = round((er - prev_er)/prev_er, 3),
           mom_n_delta = round((post_count - prev_n)/prev_n, 3),
           imp_to_bm = round((avg_impressions - bm_imp)/bm_imp, 3),
           er_to_bm = round((er - bm_er)/bm_er, 3),
           engagements = sum(engagements),
           follower_gained = NA_integer_,
           follower_delta = NA_integer_) |> 
    select(network, month, follower_gained, follower_delta, post_count, 
           mom_n_delta, impressions, avg_impressions, mom_imp_delta, imp_to_bm, 
           engagements, er, mom_er_delta, er_to_bm) %>%  
    set_names(nm = toupper(names(.)))
  
  # how did the current month's posts fall into each bucket range
  platform_thresholds <- current |> 
    get_organic_impressions() |> 
    mutate(imp = case_when(network == "TikTok" ~ video_views,
                           TRUE ~ imp),
           er = engagements/imp, 
           month = floor_date(as.Date(date, format = "%m/%d/%Y"), "month")) |> 
    left_join(thresholds) |> 
    select(network, imp, er, matches("imp_p|er_p_")) |> 
    mutate(imp_below = ifelse(imp < imp_p_5, 1, 0),
           imp_t1 = ifelse((imp > imp_p_5 & imp < imp_p_70), 1, 0),
           imp_t2 = ifelse((imp > imp_p_70 & imp < imp_p_95), 1, 0),
           imp_t3 = ifelse(imp > imp_p_95, 1, 0),
           er_below = ifelse(er < er_p_5, 1, 0),
           er_t1 = ifelse((er > er_p_5 & er < er_p_70), 1, 0),
           er_t2 = ifelse((er > er_p_70 & er < er_p_95), 1, 0),
           er_t3 = ifelse(er > er_p_95, 1, 0)) |> 
    group_by(network) |> 
    summarise(across(imp_below:er_t3, sum)) %>%  
    mutate(post_total = rowSums(.[2:ncol(.)])/2) |> 
    rowwise() |> 
    mutate(across(where(is.numeric) & !c(post_total), ~ .x / post_total, 
                  .names = "{.col}_pct"),
           month = as.Date(glue::glue("{current_year}-{current_month}-01"))) |> 
    relocate(month, .before = everything())
  
  
  # 4. POST PERFORMANCE -----------------------------------------------------
  # protect against 0 rows of data for a network
  top_bottom_n <- function(data, metric, n = 2) {
    if (nrow(data) == 0) return(data)  # <-- early return if group is empty
    
    metric <- rlang::ensym(metric)
    
    best <- data |> arrange(desc(!!metric)) |> slice_head(n = n)
    worst <- data |> arrange(!!metric) |> slice_head(n = n)
    
    bind_rows(best, worst) %>%
      mutate(rating = rep(c("Best", "Best", "Worst", "Worst"), length.out = nrow(.)))
  }
  
  bw_imp <- current |> 
    get_organic_impressions() |> 
    mutate(imp = if_else(network == "TikTok", video_views, imp),
           er = round(engagements / imp, 3),
           post_date = as.Date(date, "%m/%d/%Y"),
           month = floor_date(post_date, "month")) |> 
    group_by(network) |> 
    group_modify( ~ top_bottom_n(.x, metric = imp)) |> 
    ungroup() |> 
    left_join(select(benchmarks$bm_imp, c(network, benchmark = bm_imp))) |> 
    select(month, post_date, network, rating, post, link, impressions = imp, benchmark, er, tags) %>% 
    set_names(nm = toupper(names(.)))
  
  bw_er <- current |> 
    get_organic_impressions() |> 
    mutate(imp = if_else(network == "TikTok", video_views, imp),
           er = round(engagements / imp, 3),
           post_date = as.Date(date, "%m/%d/%Y"),
           month = floor_date(post_date, "month")) |> 
    group_by(network) |> 
    group_modify( ~ top_bottom_n(.x, metric = er)) |> 
    ungroup() |> 
    left_join(select(benchmarks$bm_er, c(network, benchmark = bm_er))) |> 
    select(month, post_date, network, rating, post, link, impressions = imp, er, benchmark, tags) %>%
    set_names(nm = toupper(names(.)))
  
  # 5. WRITE TO GOOGLE SHEETS -----------------------------------------------
  tmp <- split(platform_performance, platform_performance$NETWORK)
  
  library(googledrive)
  options(gargle_oauth_email = "taylor_grant@gspsf.com")
  id <- "1kyVKRuQcYmjnL6iSfwoBfWvPfvMnJw-sJkTWY4t6S7g"
  # googlesheets4::gs4_create(name = "Zaxby's Monthly Social", sheets = tmp) # initial create
  googlesheets4::sheet_append(monthly_totals, ss = id, sheet = "Overall")
  googlesheets4::sheet_append(tmp$Facebook, ss = id, sheet = "Facebook")
  googlesheets4::sheet_append(tmp$`Instagram Post`, ss = id, sheet = "Instagram Post")
  googlesheets4::sheet_append(tmp$`Instagram Story`, ss = id, sheet = "Instagram Story")
  googlesheets4::sheet_append(tmp$TikTok, ss = id, sheet = "TikTok")
  googlesheets4::sheet_append(tmp$X, ss = id, sheet = "X")
  googlesheets4::sheet_write(bw_imp, ss = id, sheet = "BW Impressions")
  googlesheets4::sheet_write(bw_er, ss = id, sheet = "BW ER")
  googlesheets4::sheet_append(current, ss = id, sheet = "Raw Data")
  googlesheets4::sheet_write(platform_thresholds, ss = id, sheet = "Platform Thresholds")
  
  
  # PLATFORM THRESHOLDS BUCKETS  --------------------------------------------
  bucket_plot <- function(data) {
    pacman::p_load(showtext)
    font_add_google("Barlow", "Barlow")
    showtext_auto()
    showtext_opts(dpi = 300)
    
    # source the theme for the plots 
    devtools::source_gist("https://gist.github.com/taylorgrant/1a486bccbde092d3b333a496e60049d5")
    
    bucket_counts <- platform_thresholds |> 
      select(month:er_t3) |> 
      pivot_longer(cols = -c(month, network),
                   values_to = "count")
    
    bucket_pct <- platform_thresholds |> 
      select(month, network, imp_below_pct:er_t3_pct) |> 
      pivot_longer(cols = -c(month, network),
                   values_to = "percent") |> 
      mutate(name = str_remove_all(name, "_pct"))
    
    bucket_full <- bucket_pct |> 
      left_join(bucket_counts) |> 
      mutate(name = factor(name, levels = c("imp_below", "imp_t1", "imp_t2", "imp_t3", "er_below", "er_t1", 
                                            "er_t2", "er_t3"))) |> 
      group_by(network) |> 
      mutate(total_post = sum(count)/2,
             network = glue::glue("{network}\n{total_post} posts"),
             percent = ifelse(percent == 0, NA, percent)) 
    
    imp_bucket_plot <- ggplot(data = filter(bucket_full, str_detect(name, "^imp")), 
                              aes(x = network, y = percent, 
                                  label = scales::percent(percent, accuracy = 1), 
                                  group = name, fill = name)) + 
      ggchicklet::geom_chicklet(width = .8) +
      coord_flip() + 
      geom_text(position = position_stack(vjust = 0.5, reverse = TRUE), color = "white") +
      scale_fill_manual(values = c("#b2182b", "#e97451", "#67a9cf", "#2166ac"),
                        name = NULL, 
                        labels = c("Below", "Tier 1", "Tier 2", "Tier 3")) +
      scale_y_continuous(labels = scales::percent) +
      theme_zax() + 
      theme(legend.position = "bottom",
            axis.text = element_text(size = 11, family = "Barlow")) + 
      labs(x = NULL, y = NULL, 
           title = "Posts by Benchmark Tiers - Impressions",
           caption = glue::glue("For the month of {format(unique(bucket_full$month), '%B %Y')}"))
    
    er_bucket_plot <- ggplot(data = filter(bucket_full, str_detect(name, "^er")), 
                             aes(x = network, y = percent, 
                                 label = scales::percent(percent, accuracy = 1), 
                                 group = name, fill = name)) + 
      ggchicklet::geom_chicklet(width = .8) +
      coord_flip() + 
      geom_text(position = position_stack(vjust = 0.5, reverse = TRUE), color = "white") +
      scale_fill_manual(values = c("#b2182b", "#e97451", "#67a9cf", "#2166ac"),
                        name = NULL, 
                        labels = c("Below", "Tier 1", "Tier 2", "Tier 3")) +
      scale_y_continuous(labels = scales::percent) +
      theme_zax() + 
      theme(legend.position = "bottom",
            axis.text = element_text(size = 11, family = "Barlow")) + 
      labs(x = NULL, y = NULL, 
           title = "Posts by Benchmark Tiers - Engagement Rate",
           caption = glue::glue("For the month of {format(unique(bucket_full$month), '%B %Y')}"))
    
    # save to figure folder
    ggsave(glue::glue(here::here("figures","impression_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png")),
           imp_bucket_plot,
           width = 6.4, height = 4.21)
    
    ggsave(glue::glue(here::here("figures","er_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png")),
           er_bucket_plot,
           width = 6.4, height = 4.21)
    
    # upload to google drive
    folder_id <- "1DNQai52oQlQuIRJas3N0dwBBM_lAVKXe"
    googledrive::drive_upload(glue::glue(here::here("figures","impression_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png")), 
                              path = googledrive::as_id(folder_id))
    googledrive::drive_upload(glue::glue(here::here("figures","er_bucket_{format(unique(bucket_full$month), '%b-%Y')}.png")), 
                              path = googledrive::as_id(folder_id))
  }
}
  

# NEW FUNCTION WITH BENCHMARK GRAPHS --------------------------------------
plot_benchmark_graphs <- function(sheet_id = "1kyVKRuQcYmjnL6iSfwoBfWvPfvMnJw-sJkTWY4t6S7g") {
  pacman::p_load(googlesheets4, purrr, janitor)
  sheet_names <- sheet_names(sheet_id)
  # read in all sheets
  all_sheets <- map(sheet_names, ~ read_sheet(ss = id, sheet = .x, skip = 1) |>
                      clean_names())
  names(all_sheets) <- sheet_names
  source(here::here("R","plot_function.R"))
  # run through these sheets and plot
  sheet_names[2:6] |>
    walk(plot_function)
}


# TO USE:  ----------------------------------------------------------------

# 1. Run function with data in proper location 
# monthly_performance(12)
# 2. Manually add in follower data in Sheets
# 3. Run the benchmark graphs
# plot_benchmark_graphs()
