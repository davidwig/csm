########################
# CSM Plot Functions
########################


# Load Windows Fonts (Arial)
extrafont::loadfonts(device = "win")

# Define Colors
cs_green <- rgb(0, 102, 51, maxColorValue = 255)
cs_gray <- rgb(242, 242, 242, maxColorValue = 255)
cs_darkgray <- rgb(230, 230, 230, maxColorValue = 255)
cs_axisgray <- rgb(159, 159, 159, maxColorValue = 255)
cs_orange <- "orange"
cs_lgreen <- "#32a852"

# Define Plot Theme
plot_theme <- ggplot2::theme(
  text = ggplot2::element_text(
    family = "Arial",
    face = "bold",
    color = "black",
    size = 10
    ),
  plot.title = ggplot2::element_text(
    size = 12,
    hjust = 0.5
  ),
  axis.text = ggplot2::element_text(
    family = "Arial Narrow",
    face = "plain",
    color = "black"
  ),
  panel.grid.major.y = ggplot2::element_line(
    size = 1,
    linetype = "solid",
    color = cs_darkgray
  ),
  panel.grid.major.x = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
  axis.line = ggplot2::element_line(color = cs_axisgray),
  panel.border = ggplot2::element_rect(
    color = cs_darkgray,
    fill = NA
  ),
  panel.background = ggplot2::element_rect(
    fill = cs_gray,
    color = cs_gray
  ),
  plot.caption = ggplot2::element_text(
    color = cs_green,
    face = "italic"
  ),
  axis.title.x = ggplot2::element_blank(),
  axis.title.y = ggplot2::element_blank(),
)

#' cSM Eco ggplot2 Theme
#'
#' This function imitates CSM Eco's chart theme
#'
#' @return A ggtheme object
#' @export
theme_csm <- function() plot_theme

#' cSM Eco ggplot2 Theme
#'
#' This function adds recession bars to ggplot using daily FRED recession indicator data
#' @param st_date Start date of desired data
#' @param ed_date End date of desired data
#' @param shade_color Color of recession bars
#' @return A geom_rect object
#' @export
add_rec_shade <- function(st_date, ed_date, shade_color = "darkgray") {

  quantmod::getSymbols.FRED("USRECDM", env = .GlobalEnv, return.class = "data.frame")
  recession <- USRECD
  recession$value <- recession$USRECD
  recession$date <- lubridate::ymd(rownames(recession))
  recession <- dplyr::filter(recession, date >= st_date & date <= ed_date)
  recession$diff <- recession$value - ecm::lagpad(recession$value, k = 1)
  recession <- recession[!is.na(recession$diff), ]
  recession.start <- recession[recession$diff == 1, ]$date
  recession.end <- recession[recession$diff == (-1), ]$date

  if (length(recession.start) > length(recession.end)) {
    recession.end <- c(recession.end, Sys.Date())
  }
  if (length(recession.end) > length(recession.start)) {
    recession.start <- c(min(recession$date), recession.start)
  }

  recs <- as.data.frame(cbind(recession.start, recession.end))
  recs$recession.start <- as.Date(as.numeric(recs$recession.start), origin = as.Date("1970-01-01"))
  recs$recession.end <- as.Date(recs$recession.end, origin = as.Date("1970-01-01"))

  if (nrow(recs) > 0) {
    rec_shade <- ggplot2::geom_rect(
      data = recs,
      inherit.aes = FALSE,
      aes(xmin = recession.start, xmax = recession.end, ymin = -Inf, ymax = +Inf),
      fill = shade_color,
      alpha = 0.5
    )
    return(rec_shade)
  }

}

#' CSM Green
#'
#' This function that calls CSM's green color
#' @return A string of the CSM green hexcode
#' @export
csm_green <- function() rgb(0, 102, 52, maxColorValue = 255)

#' CSM Gray
#'
#' This function that calls CSM's gray color
#' @return A string of the CSM gray hexcode
#' @export
csm_gray <- function() rgb(242, 242, 242, maxColorValue = 255)



# Test
# time <- seq.Date(as.Date("1960-03-31"), as.Date("1980-12-31"), by = "quarter")
# jj <- as.numeric(JohnsonJohnson)
# df <- data.frame(time, jj)
# ggplot(data = df) +
#   aes(x = time, y = jj) +
#   geom_line(color = csm_green(), size = 1.2) +
#   scale_x_date(date_labels = "%y") +
#   labs(title = "Johnson & Johnson Revenue \n Test") +
#   theme_csm()

