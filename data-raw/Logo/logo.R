#' Display survlab Package Logo
#'
#' This function displays the survlab package logo in the R console or viewer.
#' Useful for presentations, documentation, or just showing off your awesome package!
#'
#' @param viewer Logical, whether to display in RStudio viewer (if available).
#'               Default is TRUE. Set to FALSE to print file path only.
#'
#' @return Invisibly returns the path to the logo file
#'
#' @details
#' The logo combines elements representing:
#' \itemize{
#'   \item Laboratory flask - representing analytical laboratory work
#'   \item Survival curve - showing the statistical methodology
#'   \item Environmental colors - emphasizing the environmental focus
#'   \item Censored data points - highlighting non-detect imputation
#' }
#'
#' @examples
#' \dontrun{
#' # Display logo in viewer
#' survlab_logo()
#'
#' # Just get the logo path
#' logo_path <- survlab_logo(viewer = FALSE)
#' }
#'
#' @export
survlab_logo <- function(viewer = TRUE) {
  # Try to find logo in multiple locations
  logo_locations <- c(
    system.file("figures", "logo.svg", package = "survlab"),
    system.file("figures", "logo.png", package = "survlab"),
    file.path("man", "figures", "logo.svg"),
    file.path("man", "figures", "logo.png"),
    file.path("inst", "figures", "logo.svg")
  )

  # Find the first existing logo file
  logo_path <- NULL
  for (path in logo_locations) {
    if (file.exists(path) && nchar(path) > 0) {
      logo_path <- path
      break
    }
  }

  if (is.null(logo_path)) {
    message("survlab logo not found. Run the logo creation script:")
    message("source('R/add_logo.R')")
    return(invisible(NULL))
  }

  if (viewer) {
    # Try to display in RStudio viewer or browser
    if (requireNamespace("utils", quietly = TRUE)) {
      if (grepl("\\.svg$", logo_path)) {
        # For SVG files, create a simple HTML wrapper
        html_content <- sprintf(
          '
<!DOCTYPE html>
<html>
<head>
    <title>survlab Package Logo</title>
    <style>
        body { 
            margin: 0; 
            padding: 20px; 
            background: #f5f5f5; 
            display: flex; 
            justify-content: center; 
            align-items: center; 
            min-height: 80vh;
            font-family: Arial, sans-serif;
        }
        .logo-container {
            text-align: center;
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .logo {
            max-width: 300px;
            height: auto;
        }
        .info {
            margin-top: 20px;
            color: #666;
            font-size: 14px;
        }
    </style>
</head>
<body>
    <div class="logo-container">
        <img src="file://%s" alt="survlab Package Logo" class="logo">
        <div class="info">
            <h3>survlab Package</h3>
            <p>Survival Model-Based Imputation for Laboratory Non-Detect Data</p>
            <p><em>Environmental Engineering • Data Analysis • R Package</em></p>
        </div>
    </div>
</body>
</html>',
          normalizePath(logo_path)
        )

        temp_html <- tempfile(fileext = ".html")
        writeLines(html_content, temp_html)
        utils::browseURL(temp_html)
      } else {
        # For PNG files, just open directly
        utils::browseURL(logo_path)
      }

      message("survlab logo displayed! 🎨")
      message("Logo path: ", logo_path)
    } else {
      message("Logo path: ", logo_path)
      message("Open this file to view the logo.")
    }
  } else {
    message("Logo path: ", logo_path)
  }

  return(invisible(logo_path))
}

#' Print Package Information with Logo
#'
#' A startup-style function that displays package information along with ASCII art
#' when the package is loaded. Can be called manually or potentially used in
#' .onAttach() for package startup messages.
#'
#' @param ascii_only Logical, whether to show only ASCII art version of logo.
#'                   Default is TRUE for compatibility.
#'
#' @return Invisibly returns package information
#'
#' @examples
#' \dontrun{
#' survlab_info()
#' }
#'
#' @export
survlab_info <- function(ascii_only = TRUE) {
  if (ascii_only) {
    # ASCII art version of the hexagon logo
    cat(
      "
   ╭─────────────────╮
  ╱                   ╲
 ╱     🧪 survlab 📊    ╲
╱                       ╲
│    Survival Models     │
│   for Lab Non-Detects  │
╲                       ╱
 ╲   Environmental R    ╱
  ╲     Package        ╱
   ╰─────────────────╯

"
    )
  }

  # Package info
  pkg_info <- utils::packageDescription("survlab")

  cat("Package: survlab\n")
  cat("Version:", pkg_info$Version %||% "dev", "\n")
  cat(
    "Title:",
    pkg_info$Title %||%
      "Survival Model-Based Imputation for Laboratory Non-Detect Data",
    "\n"
  )
  cat("\nMain functions:\n")
  cat("• impute_nondetect()  - Impute non-detect values\n")
  cat("• validate_imputation() - Validate imputation results\n")
  cat("• survlab_logo()      - Display package logo\n")
  cat("\nGet started: data(multi_censored_data)\n")
  cat("Help: ?impute_nondetect\n")
  cat("Logo: survlab_logo()\n\n")

  return(invisible(pkg_info))
}

# Helper function for null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x