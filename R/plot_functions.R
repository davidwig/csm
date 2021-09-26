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
add_rec_shade <- function(st_date, shade_color = "darkgray") {
  recessions_df <- read.table(textConnection(
      "Peak, Trough
  1857-06-01, 1858-12-01
  1860-10-01, 1861-06-01
  1865-04-01, 1867-12-01
  1869-06-01, 1870-12-01
  1873-10-01, 1879-03-01
  1882-03-01, 1885-05-01
  1887-03-01, 1888-04-01
  1890-07-01, 1891-05-01
  1893-01-01, 1894-06-01
  1895-12-01, 1897-06-01
  1899-06-01, 1900-12-01
  1902-09-01, 1904-08-01
  1907-05-01, 1908-06-01
  1910-01-01, 1912-01-01
  1913-01-01, 1914-12-01
  1918-08-01, 1919-03-01
  1920-01-01, 1921-07-01
  1923-05-01, 1924-07-01
  1926-10-01, 1927-11-01
  1929-08-01, 1933-03-01
  1937-05-01, 1938-06-01
  1945-02-01, 1945-10-01
  1948-11-01, 1949-10-01
  1953-07-01, 1954-05-01
  1957-08-01, 1958-04-01
  1960-04-01, 1961-02-01
  1969-12-01, 1970-11-01
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01"), sep=',',
      colClasses=c('Date', 'Date'), header=TRUE)
  out <- ggplot2::geom_rect(data = recessions_df[recessions_df$Peak >= st_date, ],
                            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf),
                            fill=shade_color)
  return(out)
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

