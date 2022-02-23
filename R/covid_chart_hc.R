#' @title Easy helper for covid chart in highcharter
#'
#' @name covid_chart_hc
#'
#' @description
#' Create covid model chart in highcharter
#'
#' @param title the chart title (defaults to NULL)
#'
#' @import highcharter
#'
#' @export
#'
#' @examples
#' covid_chart_hc()

covid_chart_hc <- function(
  title = NULL
) {
  chart <- highchart() %>% 
    hc_chart(style = list(fontFamily = "Arial")) %>% 
    hc_add_series(data = chartdf,
                  name = "99% prediction interval",
                  type = "arearange",
                  lineWidth = 0,
                  color = rgb(66,85,99, alpha = 70, maxColorValue = 255),
                  marker = list(enabled = FALSE),
                  dataLabels = list(enabled = FALSE),
                  # enableMouseTracking = FALSE,
                  hcaes(x = MONTH_START,
                        high = signif(PRED_ITEMS_99_UPR,3),
                        low = signif(PRED_ITEMS_99_LWR,3))) %>% 
    hc_add_series(data = chartdf,
                  name = "95% prediction interval",
                  type = "arearange",
                  lineWidth = 0,
                  color = rgb(66,85,99, alpha = 140, maxColorValue = 255),
                  marker = list(enabled = FALSE),
                  dataLabels = list(enabled = FALSE),
                  hcaes(x = MONTH_START,
                        high = signif(PRED_ITEMS_95_UPR,3),
                        low = signif(PRED_ITEMS_95_LWR,3))) %>% 
    hc_add_series(data = chartdf,
                  name = "Expected items",
                  type = "line",
                  dashStyle = "Dash",
                  color = "#231f20",
                  marker = list(enabled = FALSE),
                  dataLabels = list(enabled = FALSE),
                  hcaes(x = MONTH_START,
                        y = signif(PRED_ITEMS_95_FIT,3))) %>%
    hc_add_series(data = chartdf,
                  name = "Prescribed items",
                  type = "line",
                  lineWidth = 3,
                  color = "#005EB8",
                  marker = list(enabled = FALSE),
                  dataLabels = list(enabled = FALSE),
                  hcaes(x = MONTH_START,
                        y = signif(ITEM_COUNT,3))) %>% 
    hc_xAxis(type = "datetime",
             dateTimeLabelFormats = list(month = "%b %y"),
             title = list(text = "Month")) %>% 
    hc_yAxis(title = list(text = "Volume"),
             min = 0) %>% 
    hc_title(text = title,
             style = list(fontSize = "16px",
                          fontWeight = "bold")) %>% 
    hc_legend(enabled = TRUE) %>% 
    hc_tooltip(enabled = TRUE,
               shared = TRUE,
               sort = TRUE) %>% 
    hc_credits(enabled = TRUE) %>% 
    hc_plotOptions(arearange = list(states = list(hover = list(enabled = FALSE))))
  
  # explicit return
  return(chart)
  
}
