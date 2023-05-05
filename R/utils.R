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
      htmltools::div(
        style = list(
          flexGrow = 1,
          marginLeft = "6px",
          background = background
        ),
        bar
      )
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

#' Export data helper
#'
#' @param .data Data to download
#' @param .csv_file File name
#' @param .path Path to save file
#' @param .drops Columns to drop
#'
#' @return HTML link to download data
csv_export <- function(.data = NULL,
                       .csv_file = NULL,
                       .path = params$download_path,
                       .drops = NULL) {
  # Remove columns
  my_df <- .data[, !(names(.data) %in% .drops)]
  # Save data
  readr::write_csv(my_df, tolower(paste0(.path, "/", .csv_file)))
}

#' Export plot helper
#'
#' @param .plot Plot to export
#' @param .svg_file File name
#' @param .path Path to save file
#' @param .width Width of plot
#' @param .heigth Height of plot
svg_export <- function(.plot = NULL,
                       .svg_file = NULL,
                       .path = params$download_path, 
                       width = NULL,
                       height = NULL) {
  # Save plot
  ggplot2::ggsave(
    filename = tolower(paste0(.path, "/", .svg_file)),
    plot = .plot, device = "svg", width = width, height = height
  )
}

#' Download helper
#'
#' @params .csv_file File name
#' @params .svg_file File name
export_files <- function(.data = NULL, .plot = NULL, width = NULL, height = NULL, .drops = NULL) {
  # Create file names
  .csv_file <- paste0(knitr::opts_current$get("label"), ".csv")
  .svg_file <- paste0(knitr::opts_current$get("label"), ".svg")
  # Export csv data
  csv_export(.data = .data, .csv_file, .drops = .drops)

  if (!is.null(.plot)) {
    # Export plot
    svg_export(.plot = .plot, .svg_file = .svg_file, width = width, height = height)
    htmltools::div(
      style = "text-align: right",
      htmltools::tags$small(
        htmltools::p(
          "Download: ",
          htmltools::tags$a(
            href = .csv_file, "Data (.csv)"
          ),
          " | ",
          htmltools::tags$a(
            href = .svg_file, "Plot (.svg)"
          )
        )
      )
    )
  } else {
    htmltools::div(
      style = "text-align: right",
      htmltools::tags$small(
        htmltools::p(
          "Download: ",
          htmltools::tags$a(
            href = .csv_file, "Data (.csv)"
          )
        )
      )
    )
  }
}