# Package Build Script for survlab
# Run this script to build and check the package

# Install required development packages
# install.packages(c("devtools", "roxygen2", "testthat", "knitr", "rmarkdown"))

# Install optional logo creation packages
# optional_packages <- c("hexSticker", "rsvg", "magick", "showtext")
# for (pkg in optional_packages) {
#   if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
#     try(install.packages(pkg), silent = TRUE)
#   }
# }

# Load development tools
library(devtools)
library(roxygen2)

# Set working directory to package root
# setwd("/path/to/survlab")

# Step 1: Create package structure (if starting from scratch)
# create_package("survlab")

# Step 2: Copy your CSV data to data-raw/ directory
# Make sure multi_censored_lognormal_data.csv is in data-raw/

# Step 3: Create package logo
cat("Creating package logo...\n")
if (file.exists("R/add_logo.R")) {
  source("R/add_logo.R")
} else {
  cat("⚠ Logo creation script not found. Skipping logo creation.\n")
  cat("  Create R/add_logo.R to enable logo generation.\n")
}

# Step 4: Prepare package data
if (file.exists("data-raw/prepare_data.R")) {
  source("data-raw/prepare_data.R")
} else {
  cat("⚠ Data preparation script not found.\n")
}

# Step 5: Generate documentation from roxygen2 comments
document()

# Step 6: Install the package locally
install()

# Step 7: Load and test the package
library(survlab)

# Quick test
data(multi_censored_data)
set.seed(123)
result <- impute_nondetect(
  multi_censored_data,
  parameter_col = "parameter",
  unit_col = "unit"
)
validate_imputation(result)

# Show logo if available
cat("\nTesting logo display...\n")
tryCatch(
  {
    survlab_info()
    survlab_logo(viewer = FALSE) # Just show path, don't open browser during build
  },
  error = function(e) {
    cat("Logo display test failed:", e$message, "\n")
  }
)

# Step 8: Run package checks (can skip vignettes if having issues)
check()
# Alternative if vignette issues: check(vignettes = FALSE)

# Step 9: Run tests
test()

# Step 10: Try to build vignettes (if this fails, skip and use alternative below)
tryCatch(
  {
    build_vignettes()
    cat("✓ Vignettes built successfully!\n")
  },
  error = function(e) {
    cat("⚠ Vignette building failed. Building package without vignettes...\n")
    cat("Error:", e$message, "\n")
  }
)

# Step 11: Build source package
build()
# Alternative without vignettes: build(vignettes = FALSE)

# Step 12: Install from built package
# install.packages("survlab_0.1.0.tar.gz", repos = NULL, type = "source")

cat("\n=== Package build complete! ===\n")
cat("Package structure created successfully.\n")
cat("Logo files created (if dependencies available).\n")
cat("To publish to GitHub:\n")
cat("1. Initialize git repository: git init\n")
cat("2. Add files: git add .\n")
cat("3. Commit: git commit -m 'Initial package commit with logo'\n")
cat(
  "4. Add remote: git remote add origin https://github.com/yourusername/survlab.git\n"
)
cat("5. Push: git push -u origin main\n")
cat("\nTo display your awesome logo: survlab_logo()\n")