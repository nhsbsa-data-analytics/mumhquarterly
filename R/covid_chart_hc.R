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
    mutate(
      ACT = prettyNum(signif(ITEM_COUNT, 3), big.mark = ","),
      EXP = prettyNum(signif(PRED_ITEMS_95_FIT, 3), big.mark = ","),
      RANGE_95 = paste(
        prettyNum(signif(PRED_ITEMS_95_LWR, 3), big.mark = ","),
        "-",
        prettyNum(signif(PRED_ITEMS_95_UPR, 3), big.mark = ",")
      ),
      RANGE_99 = paste(
        prettyNum(signif(PRED_ITEMS_99_LWR, 3), big.mark = ","),
        "-",
        prettyNum(signif(PRED_ITEMS_99_UPR, 3), big.mark = ",")
      )
    )

  chart <- highchart() %>%
    hc_chart(style = list(fontFamily = "Arial")) %>%
    hc_add_series(
      data = chart_data,
      name = "99% prediction interval",
      type = "arearange",
      lineWidth = 0,
      color = rgb(66, 85, 99, alpha = 70, maxColorValue = 255),
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      # enableMouseTracking = FALSE,
      hcaes(
        x = MONTH_START,
        high = signif(PRED_ITEMS_99_UPR, 3),
        low = signif(PRED_ITEMS_99_LWR, 3),
        tooltip = RANGE_99
      )
    ) %>%
    hc_add_series(
      data = chart_data,
      name = "95% prediction interval",
      type = "arearange",
      lineWidth = 0,
      color = rgb(66, 85, 99, alpha = 140, maxColorValue = 255),
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(
        x = MONTH_START,
        high = signif(PRED_ITEMS_95_UPR, 3),
        low = signif(PRED_ITEMS_95_LWR, 3),
        tooltip = RANGE_95
      )
    ) %>%
    hc_add_series(
      data = chart_data,
      name = "Expected items",
      type = "line",
      dashStyle = "Dash",
      color = "#231f20",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(x = MONTH_START,
            y = signif(PRED_ITEMS_95_FIT, 3),
            tooltip = EXP)
    ) %>%
    hc_add_series(
      data = chart_data,
      name = "Prescribed items",
      type = "line",
      lineWidth = 3,
      color = "#005EB8",
      marker = list(enabled = FALSE),
      dataLabels = list(enabled = FALSE),
      hcaes(x = MONTH_START,
            y = signif(ITEM_COUNT, 3),
            tooltip = ACT)
    ) %>%
    hc_xAxis(type = "datetime",
             dateTimeLabelFormats = list(month = "%b %y"),
             title = list(text = "Month")) %>%
    hc_yAxis(title = list(text = "Volume"),
             min = 0) %>%
    hc_title(text = title,
             style = list(fontSize = "16px",
                          fontWeight = "bold")) %>%
    hc_legend(enabled = TRUE,
              reversed = TRUE) %>%
    hc_tooltip(
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
    hc_credits(enabled = TRUE) %>%
    hc_plotOptions(arearange = list(states = list(hover = list(enabled = FALSE))))


  # explicit return
  return(chart)

}
