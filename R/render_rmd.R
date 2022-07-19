#' Create reports
#'
#' @param rmd_filename string: name of Rmd file
#' @param output_dir string: optional path to output directory. Defaults
#'   to "~" directory
#' @param save_artefacts boolean: define whether plots and tables should be
#'   saved as separate files in the output_dir
#' @param plot_type string: png or svg
#'
#' @export
#'
render_rmd <- function(rmd_filename,
                       output_dir = path.expand("~"),
                       save_artefacts = FALSE,
                       plot_type = c("png", "svg")) {
  plot_type <- match.arg(plot_type)

  stopifnot(
    is.logical(save_artefacts),
    is.character(rmd_filename),
    is.character(output_dir)
  )

  path <- system.file("rmd", rmd_filename,
    package = "covidchildtb"
  )

  if (path == "") {
    stop("File not found.\n Try: list.files(system.file(\"rmd\", package = \"covidchildtb\"))")
  }

  rmarkdown::render(
    input = path,
    output_dir = output_dir,
    params = list(
      save_artefacts = save_artefacts,
      plot_type = plot_type
    )
  )
}