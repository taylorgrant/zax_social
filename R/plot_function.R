plot_function <- function(platform) {
  
  # load font
  pacman::p_load(showtext)
  font_add_google("Barlow", "Barlow")
  showtext_auto()
  showtext_opts(dpi = 300)
  
  # source the theme for the plots 
  devtools::source_gist("https://gist.github.com/taylorgrant/1a486bccbde092d3b333a496e60049d5")
  
  # pull in benchmarks 
  benchmarks <- readRDS(here::here("data", "benchmarks.rds"))
  
  # function to format a number 
  format_number <- function(x) {
    case_when(
      abs(x) >= 1e9 ~ paste0(round(x / 1e9, 1), "B"), # Billions
      abs(x) >= 1e6 ~ paste0(round(x / 1e6, 1), "M"), # Millions
      abs(x) >= 1e3 ~ paste0(round(x / 1e3, 1), "k"), # Thousands
      TRUE ~ as.character(x)                          # No truncation
    )
  }
  
  # plot 1 - average impressions per post
  i_label <- paste0(format_number(all_sheets[[platform]]$avg_impressions),"\n", 
                    ifelse(all_sheets[[platform]]$imp_to_bm > 0, 
                           paste0("(+",scales::percent(all_sheets[[platform]]$imp_to_bm, accuracy = 1), ")"), 
                           paste0("(", scales::percent(all_sheets[[platform]]$imp_to_bm, accuracy = 1), ")")))
  i_labels <- ifelse(seq_along(i_label) == length(i_label), i_label, NA)
  p1 <- ggplot(all_sheets[[platform]], aes(x = month, y = avg_impressions)) +
    ggchicklet::geom_chicklet(fill = "#4284f3") + 
    geom_text(aes(label = i_labels), vjust = -.2, na.rm = TRUE, size = 4, family = "Gothic") +
    scale_x_datetime(date_breaks = "months", date_labels = "%b %y") +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.2))) + 
    geom_hline(yintercept = filter(benchmarks$bm_imp, network == platform)$bm_imp,
               linetype = "dashed", color = '#E4012A') +
    # geom_hline(yintercept = filter(benchmarks$bm_t1imp, network == platform)$bm_t1imp,
    #            linetype = "dashed", color = '#FC8E1C') +
    theme_zax(grid = "Y") + 
    theme(axis.text = element_text(size = 11),
          plot.title = element_text(size = 14)) +
    labs(x = "Month", y = NULL,
         title = "Avg. Impressions",
         caption = glue::glue("Benchmark for {platform}: {scales::comma(benchmarks$bm_imp[benchmarks$bm_imp$network == platform,]$bm_imp)}\nTier 1 benchmark for {platform}: {scales::comma(benchmarks$bm_t1imp[benchmarks$bm_t1imp$network == platform,]$bm_t1imp)}"))
  
  # plot 2 - average engagement rate per post
  e_label <- paste0(scales::percent(all_sheets[[platform]]$er, accuracy = .1),"\n",
                    ifelse(all_sheets[[platform]]$er_to_bm > 0,
                           paste0("(+",scales::percent(all_sheets[[platform]]$er_to_bm, accuracy = 1), ")"),
                           paste0("(", scales::percent(all_sheets[[platform]]$er_to_bm, accuracy = 1), ")")))
  e_labels <- ifelse(seq_along(e_label) == length(e_label), e_label, NA)
  p2 <- ggplot(all_sheets[[platform]], aes(x = month, y = er)) +
    ggchicklet::geom_chicklet(fill = "#4284f3") +
    geom_text(aes(label = e_labels), vjust = -.2, na.rm = TRUE, size = 4, family = "Gothic") +
    scale_x_datetime(date_breaks = "months", date_labels = "%b %y") +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.2))) +
    geom_hline(yintercept = filter(benchmarks$bm_er, network == platform)$bm_er,
               linetype = "dashed", color = '#E4012A') +
    # geom_hline(yintercept = filter(benchmarks$bm_t1er, network == platform)$bm_t1er,
    #            linetype = "dashed", color = '#FC8E1C') +
    theme_zax(grid = "Y") +
    theme(axis.text = element_text(size = 11),
          plot.title = element_text(size = 14)) +
    labs(x = "Month", y = NULL,
         title = "Engagement Rate",
         caption = glue::glue("Benchmark for {platform}: {scales::percent(benchmarks$bm_er[benchmarks$bm_er$network == platform,]$bm_er, accuracy = .1)}\nTier 1 benchmark for {platform}: {scales::percent(benchmarks$bm_t1er[benchmarks$bm_t1er$network == platform,]$bm_t1er, accuracy = .1)}"))
  
  # plot 3 - followers gained lost 
  if (platform != "Instagram Story") {
    
    f_label <- paste0(format_number(all_sheets[[platform]]$follower_gained),"\n", 
                      ifelse(all_sheets[[platform]]$follower_delta > 0, 
                             paste0("(+",scales::percent(all_sheets[[platform]]$follower_delta, accuracy = 1), ")"), 
                             paste0("(", scales::percent(all_sheets[[platform]]$follower_delta, accuracy = 1), ")")))
    f_labels <- ifelse(seq_along(f_label) == length(f_label), f_label, NA)
    tmpdat <- all_sheets[[platform]] |> 
      mutate(txtcol = ifelse(follower_gained < 0, "white", "black"))
    
    p3 <- ggplot(tmpdat, aes(x = month, y = follower_gained)) +
      ggchicklet::geom_chicklet(fill = "#4284f3") + 
      geom_text(aes(label = f_labels), vjust = -.2, na.rm = TRUE, size = 4, family = "Gothic",
                color = tmpdat$txtcol) +
      scale_x_datetime(date_breaks = "months", date_labels = "%b %y") +
      scale_y_continuous(labels = scales::comma,
                         expand = expansion(mult = c(0, 0.2))) + 
      # geom_hline(yintercept = filter(benchmarks$bm_imp, network == platform)$bm_imp,
      #            linetype = "dashed", color = '#E4012A') +
      theme_zax(grid = "Y") + 
      theme(axis.text = element_text(size = 11),
            plot.title = element_text(size = 14)) +
      labs(x = "Month", y = NULL,
           title = "Followers Gained / Lost",
           caption = "Percentages based on a month over month calculation")
    
    library(patchwork)
    pfig <- (p1 | p2 | p3) & theme(plot.background = element_rect(fill = "transparent", color = "transparent"))
  } else {
    library(patchwork)
    pfig <- (p1 | p2) & theme(plot.background = element_rect(fill = "transparent", color = "transparent"))
  }
  
  # save
  ggsave(glue::glue("~/R/zaxbys/social_2024/figures/{platform}-{as.Date(last(all_sheets[[platform]]$month))}.png"),
         width = 12.4, height = 4.21)
  # upload to google drive
  folder_id <- "1DNQai52oQlQuIRJas3N0dwBBM_lAVKXe"
  googledrive::drive_upload(glue::glue("~/R/zaxbys/social_2024/figures/{platform}-{as.Date(last(all_sheets[[platform]]$month))}.png"), path = googledrive::as_id(folder_id))
  
}

