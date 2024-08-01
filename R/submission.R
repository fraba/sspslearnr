#' @title Tutorial submission functions
#'
#' @description
#' The following function has modified from Colin
#' Rundel's learnrhash package, available at
#' https://github.com/rundel/learnrhash. Many thanks to Professor Rundel, who
#' has developed a fantastic tool for courses that teach R and use the learnr
#' package.
#'
#' This note is also modified from Professor Rundel's description: Note that when
#' including these functions in a learnr Rmd document it is necessary that the
#' server function, `submission_server()`, be included in an R chunk where
#' `context="server"`. Conversely, any of the ui functions, `*_ui()`, must *not*
#' be included in an R chunk with a `context`.
#'
#' @rdname submission_functions
#' @export
submission_server <- function(input, output) {
  p <- parent.frame()
  check_server_context(p)

  # Evaluate in parent frame to get input, output, and session
  local({
    build_report <- function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "tutorial-report.Rmd")
      tut_rep_path <- file.path(path.package("sspslearnr"),
                                "tutorials",
                                "tutorial-report.Rmd")
      file.copy(tut_rep_path, tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      objs2 = learnr:::get_tutorial_state()
      
      ##browser()
      num_correct <- 
        # Access the $correct sublist item in each list item
        lapply(objs2, purrr::pluck, "correct") |>
        # make it a vector containing: TRUE and FALSE and NAs
        # NA is appearing for list items which don't have
        # a $correct subitem
        unlist() |> 
        # Taking the sum of a logical Vector returns the number of TRUEs
        sum(na.rm=TRUE)
      
      # Number of questions/exercises answered
      num_answered <- 
        # 1. Access $type in each list item and make it a vector of types
        lapply(objs2, purrr::pluck, "type") |> unlist()
      
      # 2. Count the number of "question" and "exercise" in that vector
      num_answered <- num_answered[num_answered == "question" |
                                     num_answered =="exercise"] |> length()
      params <- list(reporttitle = tut_reptitle,
                     num_answered = num_answered,
                     num_correct = num_correct,
                     num_tutorial = num_tutorial,
                     student_name = input$name,
                     student_unikey = input$unikey)
      
      ext <- tools::file_ext(file)
      out_format <- switch(ext, pdf = "pdf_document", html = "html_document")

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(
        tempReport,
        output_format = out_format,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
    output$download_pdf <- downloadHandler(
      filename = "report.pdf",
      content = build_report
    )
    
    output$download_html <- downloadHandler(
      filename = "report.html",
      content = build_report
    )
  }, envir = p)
}

check_server_context <- function(.envir) {
  if (!is_server_context(.envir)) {
    calling_func <- deparse(sys.calls()[[sys.nframe() - 1]])

    err <- paste0(
      "Function `", calling_func, "`",
      " must be called from an Rmd chunk where `context = \"server\"`"
    )

    stop(err, call. = FALSE)
  }
}

is_server_context <- function(.envir) {
  # We are in the server context if there are the follow:
  # * input - input reactive values
  # * output - shiny output
  # * session - shiny session
  #
  # Check context by examining the class of each of these.
  # If any is missing then it will be a NULL which will fail.

  inherits(.envir$input,   "reactivevalues") &
    inherits(.envir$output,  "shinyoutput")    &
    inherits(.envir$session, "ShinySession")
}

#' @rdname submission_functions
#' @export
submission_ui <- shiny::div(
  "When you have completed this tutorial, follow these steps:",
  shiny::tags$br(),
  shiny::tags$ol(
    shiny::tags$li("Enter your name and UniKey (should look like: abcd1234) into the text box below.."),
    shiny::tags$li("Click the Download button next to generate a report PDF with a summary of your work. "),
    shiny::tags$li("Upload this file to the appropriate assignment on Gradescope.")),
  shiny::textInput("name", "Your Name"),
  shiny::textInput("unikey", "UniKey"),
  shiny::downloadButton(outputId = "download_pdf", label = "Download PDF"),
  shiny::downloadButton(outputId = "download_html", label = "Download HTML (backup)")
)

utils::globalVariables(c("input", "session", "download_report"))
