follower_plot <- function(file_location) {
  # pacman::p_load(tidyverse, janitor, here, glue, showtext)

  # load font
  sysfonts::font_add_google("Barlow", "Barlow")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)

  # source the zax theme for the plots
  devtools::source_gist(
    "https://gist.github.com/taylorgrant/1a486bccbde092d3b333a496e60049d5"
  )

  # read in the data
  dat <- readr::read_csv(file_location) |>
    janitor::clean_names() |>
    dplyr::select(date, network, profile, audience) |>
    dplyr::mutate(date = as.Date(date, format = "%m-%d-%Y")) |>
    dplyr::mutate(
      year = lubridate::year(date),
      month = lubridate::month(date)
    ) |>
    dplyr::group_by(network, year, month) |>
    dplyr::filter(date == max(date)) |>
    dplyr::filter(
      # if not the last day of month, drop that month
      !(year == lubridate::year(Sys.Date()) &
        month == lubridate::month(Sys.Date()) &
        date !=
          lubridate::ceiling_date(Sys.Date(), "month") - lubridate::days(1))
    ) |>
    dplyr::ungroup()

  # filter so data is rolling 12 month
  dat <- dat |>
    # dplyr::filter(date >= max(dat$date) - lubridate::years(1))
    dplyr::filter(date >= "2025-01-31")

  # plot function
  make_plot <- function(net) {
    x_min <- lubridate::floor_date(min(dat$date), "month")
    x_max <- lubridate::ceiling_date(max(dat$date), "month") -
      lubridate::days(1)

    # filter to the network and shift date to the floor
    tmp <- dat |>
      dplyr::filter(network == net) |>
      dplyr::mutate(month = lubridate::floor_date(as.Date(date), "month"))

    # plot
    p <- ggplot2::ggplot(tmp, ggplot2::aes(x = month, y = audience)) +
      ggchicklet::geom_chicklet(fill = "#4284f3", width = 25) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::comma(audience)),
        vjust = .5,
        hjust = 1.1,
        na.rm = TRUE,
        size = 4,
        family = "Barlow",
        color = "white",
        angle = 90
      ) +
      ggplot2::scale_x_date(
        breaks = seq(min(tmp$month), max(tmp$month), by = "1 month"),
        # date_breaks = "1 month",
        date_labels = "%b\n%y",
        # limits = c(x_min, x_max),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.2))
      ) +
      theme_zax(grid = FALSE, ticks = TRUE) +
      ggplot2::labs(
        x = NULL,
        y = NULL,
        title = net,
        caption = "Bars capture month-end follower counts"
      ) +
      ggplot2::theme(
        plot.title.position = "plot",
        plot.background = ggplot2::element_rect(
          fill = "transparent",
          color = NA
        ),
        panel.background = ggplot2::element_rect(
          fill = "transparent",
          color = NA
        )
      )

    # save plot
    ggplot2::ggsave(
      filename = glue::glue(
        "~/R/zaxbys/social_2024/figures/{net}-followers-{lubridate::month(x_max)}-{lubridate::year(x_max)}.png"
      ),
      plot = p,
      width = 8,
      height = 5,
      bg = "transparent"
    )
    # upload to google drive
    folder_id <- "1Okf2ALjrTJhBF47WuK4tquVKjPcKVWoN"
    # upload to google drive
    googledrive::drive_upload(
      glue::glue(
        "~/R/zaxbys/social_2024/figures/{net}-followers-{lubridate::month(x_max)}-{lubridate::year(x_max)}.png"
      ),
      path = googledrive::as_id(folder_id)
    )
  }
  nn <- c("X", "Facebook", "Instagram", "TikTok")
  nn |>
    purrr::walk(make_plot)
}

file_location <- "FOLDER/WITH/FOLLOWER/DATA" # this is a csv file
follower_plot(file_location)
