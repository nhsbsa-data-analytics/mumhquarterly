#' @title Easy helper for covid chart in highcharter
#'
#' @name covid_chart_hc
#'
#' @description
#' Create covid model chart in highcharter
#'
#' @param data a dataframe to be charted
#' @param title the chart title (defaults to NULL)
#'
#' @import highcharter
#'
#' @export
#'
#' @examples
#' covid_chart_hc()

covid_chart_hc <- function(
  data,
  title = NULL
) {
  chart_data <- data %>%
    dplyr::mutate(
      ACT = prettyNum(signif(total_items, 3), big.mark = ","),
      EXP = prettyNum(signif(mean_fit, 3), big.mark = ","),
      RANGE_95 = paste(
        prettyNum(signif(PIlwr, 3), big.mark = ","),
        "-",
        prettyNum(signif(PIupr, 3), big.mark = ",")
      ),
      RANGE_99 = paste(
        prettyNum(signif(PIlwr99, 3), big.mark = ","),
        "-",
        prettyNum(signif(PIupr99, 3), big.mark = ",")
      ),
      MONTH_START = as.Date(paste0(YEAR_MONTH_string, "01"), format = "%Y%m%d")
    )

  chart <- highchart() %>%
    highcharter::hc_chart(style = list(fontFamily = "Arial")) %>%
    highcharter::hc_add_series(
      data = chart_data,
      name = "99% prediction interval",
      type = "arearange",
      lineWidth = 0,
      color = "#425563",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      # enableMouseTracking = FALSE,
      highcharter::hcaes(
        x = MONTH_START,
        high = signif(PIupr99, 3),
        low = signif(PIlwr99, 3),
        tooltip = RANGE_99
      )
    ) %>%
    highcharter::hc_add_series(
      data = chart_data,
      name = "95% prediction interval",
      type = "arearange",
      lineWidth = 0,
      color = "#b3bbc1",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(
        x = MONTH_START,
        high = signif(PIupr, 3),
        low = signif(PIlwr, 3),
        tooltip = RANGE_95
      )
    ) %>%
    highcharter::hc_add_series(
      data = chart_data,
      name = "Expected items",
      type = "line",
      dashStyle = "Dash",
      color = "#231f20",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(x = MONTH_START,
            y = signif(mean_fit, 3),
            tooltip = EXP)
    ) %>%
    highcharter::hc_add_series(
      data = chart_data,
      name = "Prescribed items",
      type = "line",
      lineWidth = 3,
      color = "#005EB8",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(x = MONTH_START,
            y = signif(total_items, 3),
            tooltip = ACT)
    ) %>%
    highcharter::hc_xAxis(type = "datetime",
             dateTimeLabelFormats = list(month = "%b %y"),
             title = list(text = "Month")) %>%
    highcharter::hc_yAxis(title = list(text = "Volume"),
             min = 0) %>%
    highcharter::hc_title(text = title,
             style = list(fontSize = "16px",
                          fontWeight = "bold")) %>%
    highcharter::hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    highcharter::hc_tooltip(
      enabled = TRUE,
      shared = TRUE,
      useHTML = TRUE,
      formatter = JS(
        "function () {
        var timeStamp = this.x;
        var dateFormat = new Date(timeStamp);
        var month = dateFormat.toLocaleString('default', { month: 'long' });
        var year = dateFormat.getFullYear();

        var s = month + ' ' + year;

        $.each(this.points.reverse(), function () {
            var number = this.point.tooltip;

            s += '<br/><span style=\"color:' + this.series.color + '\">\u25CF</span> ' + this.series.name + ': ' +
                '<b>' + number + '</b>';
        });

        return s;
    }"
      )
    ) %>%
    highcharter::hc_credits(enabled = TRUE) %>%
    highcharter::hc_plotOptions(arearange = list(states = list(hover = list(enabled = FALSE))))


  # explicit return
  return(chart)

}
