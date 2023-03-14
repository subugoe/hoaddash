#' React bar chart helper
#'
#' From <https://glin.github.io/reactable/articles/building-twitter-followers.html>
#'
#' @importFrom htmltools div
#'
#' @noRd
react_bar_chart <-
  function(label,
           width = "100%",
           height = "14px",
           fill = "#00bfc4",
           background = NULL) {
    bar <-
      htmltools::div(style = list(
        background = fill,
        width = width,
        height = height
      ))
    chart <-
      htmltools::div(style = list(
        flexGrow = 1,
        marginLeft = "6px",
        background = background
      ),
      bar)
    htmltools::div(
      style = list(
        display = "flex",
        alignItems = "center",
        justifyContent = "flex-end"
      ),
      label,
      chart
    )
  }
