# Script to create and add logo to survlab package
# Run this script to generate logo files and integrate them into the package

# Install required packages if not already installed
if (!require("hexSticker")) {
  install.packages("hexSticker")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
if (!require("showtext")) {
  install.packages("showtext")
}

library(hexSticker)
library(ggplot2)
library(showtext)

# Create directory for logo files
if (!dir.exists("man/figures")) {
  dir.create("man/figures", recursive = TRUE)
}
if (!dir.exists("inst/figures")) {
  dir.create("inst/figures", recursive = TRUE)
}

# Method 1: Create logo using hexSticker package (alternative approach)
create_hexsticker_logo <- function() {
  # Create a simple survival curve plot
  set.seed(123)
  x <- seq(0, 10, length.out = 50)
  survival_prob <- exp(-0.1 * x) * (1 - 0.3 * (x > 5))
  survival_data <- data.frame(time = x, survival = survival_prob)

  # Create survival curve plot
  p <- ggplot(survival_data, aes(x = time, y = survival)) +
    geom_step(color = "#FFD700", size = 1.5) +
    geom_hline(
      yintercept = 0.3,
      linetype = "dashed",
      color = "#FF6B6B",
      size = 1
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )

  # Create hexagon sticker
  sticker(
    subplot = p,
    package = "survlab",
    p_size = 24,
    p_color = "white",
    p_family = "sans",
    p_fontface = "bold",
    p_y = 1.45,
    s_x = 1,
    s_y = 0.85,
    s_width = 1.2,
    s_height = 0.8,
    h_fill = "#2E8B57",
    h_color = "#1B5E3F",
    h_size = 2,
    url = "Environmental Lab Data",
    u_color = "white",
    u_size = 3,
    u_y = 0.15,
    filename = "man/figures/logo.png",
    dpi = 300
  )

  cat("✓ Logo created using hexSticker: man/figures/logo.png\n")
}

# Method 2: Save the custom SVG logo (recommended)
save_svg_logo <- function() {
  # SVG logo content
  svg_content <- '<?xml version="1.0" encoding="UTF-8"?>
<svg width="200" height="231" viewBox="0 0 200 231" xmlns="http://www.w3.org/2000/svg">
  <!-- Hexagon background -->
  <defs>
    <linearGradient id="bgGradient" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#2E8B57;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#1B5E3F;stop-opacity:1" />
    </linearGradient>
    <linearGradient id="accentGradient" x1="0%" y1="0%" x2="100%" y2="100%">
      <stop offset="0%" style="stop-color:#4A90E2;stop-opacity:1" />
      <stop offset="100%" style="stop-color:#2C5F8A;stop-opacity:1" />
    </linearGradient>
  </defs>
  
  <!-- Main hexagon -->
  <polygon points="100,15 173,57.5 173,142.5 100,185 27,142.5 27,57.5" 
           fill="url(#bgGradient)" 
           stroke="#1B5E3F" 
           stroke-width="3"/>
  
  <!-- Laboratory flask/beaker icon -->
  <g transform="translate(100,70)">
    <!-- Flask body -->
    <path d="M-15,-25 L-15,-35 L-8,-35 L-8,-45 L8,-45 L8,-35 L15,-35 L15,-25 L25,5 Q25,15 15,15 L-15,15 Q-25,15 -25,5 Z" 
          fill="#E8F4FD" 
          stroke="#4A90E2" 
          stroke-width="2"/>
    
    <!-- Flask neck -->
    <rect x="-8" y="-45" width="16" height="10" fill="#E8F4FD" stroke="#4A90E2" stroke-width="2"/>
    
    <!-- Liquid in flask -->
    <path d="M-12,-20 L12,-20 L20,0 Q20,8 12,8 L-12,8 Q-20,8 -20,0 Z" 
          fill="url(#accentGradient)" 
          opacity="0.7"/>
    
    <!-- Measurement lines on flask -->
    <line x1="-18" y1="-10" x2="-12" y2="-10" stroke="#4A90E2" stroke-width="1"/>
    <line x1="-18" y1="-5" x2="-15" y2="-5" stroke="#4A90E2" stroke-width="1"/>
    <line x1="-18" y1="0" x2="-12" y2="0" stroke="#4A90E2" stroke-width="1"/>
  </g>
  
  <!-- Survival curve/statistical element -->
  <g transform="translate(100,120)">
    <!-- Grid background -->
    <rect x="-25" y="-10" width="50" height="25" fill="none" stroke="#98D8C8" stroke-width="0.5" opacity="0.3"/>
    <line x1="-25" y1="0" x2="25" y2="0" stroke="#98D8C8" stroke-width="0.5" opacity="0.3"/>
    <line x1="0" y1="-10" x2="0" y2="15" stroke="#98D8C8" stroke-width="0.5" opacity="0.3"/>
    
    <!-- Survival curve (step function typical of Kaplan-Meier) -->
    <path d="M-25,10 L-15,10 L-15,5 L-5,5 L-5,0 L5,0 L5,-5 L15,-5 L15,-8 L25,-8" 
          fill="none" 
          stroke="#FFD700" 
          stroke-width="2.5"/>
    
    <!-- Detection limit line (censoring indicator) -->
    <line x1="-25" y1="2" x2="25" y2="2" stroke="#FF6B6B" stroke-width="1.5" stroke-dasharray="3,2"/>
    
    <!-- Data points -->
    <circle cx="-15" cy="10" r="1.5" fill="#FFD700"/>
    <circle cx="-5" cy="5" r="1.5" fill="#FFD700"/>
    <circle cx="5" cy="0" r="1.5" fill="#FFD700"/>
    <circle cx="15" cy="-5" r="1.5" fill="#FFD700"/>
    
    <!-- Censored points (different symbol) -->
    <rect x="-20" y="1" width="2" height="2" fill="#FF6B6B" transform="rotate(45 -19 2)"/>
    <rect x="-10" y="1" width="2" height="2" fill="#FF6B6B" transform="rotate(45 -9 2)"/>
    <rect x="0" y="1" width="2" height="2" fill="#FF6B6B" transform="rotate(45 1 2)"/>
  </g>
  
  <!-- Package name -->
  <text x="100" y="205" text-anchor="middle" font-family="Arial, sans-serif" font-size="20" font-weight="bold" fill="white">
    survlab
  </text>
  
  <!-- Tagline -->
  <text x="100" y="220" text-anchor="middle" font-family="Arial, sans-serif" font-size="8" fill="#98D8C8">
    ENVIRONMENTAL LAB DATA
  </text>
</svg>'

  # Save SVG logo
  writeLines(svg_content, "man/figures/logo.svg")
  writeLines(svg_content, "inst/figures/logo.svg")

  cat("✓ SVG logo saved to:\n")
  cat("  - man/figures/logo.svg\n")
  cat("  - inst/figures/logo.svg\n")
}

# Method 3: Convert SVG to PNG using various methods
convert_svg_to_png <- function() {
  # Try different conversion methods
  svg_file <- "man/figures/logo.svg"
  png_file <- "man/figures/logo.png"

  if (file.exists(svg_file)) {
    # Method 3a: Using rsvg package (recommended)
    if (require("rsvg", quietly = TRUE)) {
      tryCatch(
        {
          rsvg::rsvg_png(svg_file, png_file, width = 400, height = 462)
          cat("✓ PNG logo created using rsvg: man/figures/logo.png\n")
          return(TRUE)
        },
        error = function(e) {
          cat("⚠ rsvg conversion failed:", e$message, "\n")
        }
      )
    }

    # Method 3b: Using magick package
    if (require("magick", quietly = TRUE)) {
      tryCatch(
        {
          logo <- magick::image_read_svg(svg_file, width = 400)
          magick::image_write(logo, png_file)
          cat("✓ PNG logo created using magick: man/figures/logo.png\n")
          return(TRUE)
        },
        error = function(e) {
          cat("⚠ magick conversion failed:", e$message, "\n")
        }
      )
    }

    # Method 3c: Manual instruction
    cat("⚠ Automatic conversion failed. Please manually convert SVG to PNG:\n")
    cat("  - Open man/figures/logo.svg in a web browser\n")
    cat("  - Take a screenshot or use online SVG to PNG converter\n")
    cat("  - Save as man/figures/logo.png (400x462 pixels recommended)\n")
  } else {
    cat("⚠ SVG file not found. Run save_svg_logo() first.\n")
  }
}

# Main execution function
create_package_logo <- function() {
  cat("Creating survlab package logo...\n\n")

  # Create the SVG logo (primary method)
  save_svg_logo()

  # Try to convert to PNG
  convert_svg_to_png()

  # Alternative: create using hexSticker if preferred
  # create_hexsticker_logo()

  cat("\n=== Logo Integration Complete! ===\n")
  cat("Files created:\n")
  cat("- man/figures/logo.svg (for README and documentation)\n")
  cat("- man/figures/logo.png (if conversion successful)\n")
  cat("- inst/figures/logo.svg (for package installation)\n")
  cat("\nNext steps:\n")
  cat("1. Update README.md to include the logo\n")
  cat("2. Optionally set up pkgdown for a package website\n")
  cat("3. Consider adding to package startup message\n")
}

# Run the logo creation
create_package_logo()