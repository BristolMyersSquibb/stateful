#!/usr/bin/env Rscript

# Create standalone Shiny app for deployment
# This bundles all stateful code into the app directory

library(rsconnect)

# Create deployment directory
deploy_dir <- "stateful_standalone"
if (dir.exists(deploy_dir)) {
  unlink(deploy_dir, recursive = TRUE)
}
dir.create(deploy_dir)

# Copy R source files
dir.create(file.path(deploy_dir, "R"))
file.copy(list.files("R", pattern = "\\.R$", full.names = TRUE),
          file.path(deploy_dir, "R"))

# Copy sample data
dir.create(file.path(deploy_dir, "data"))
file.copy("inst/extdata/rt-dm-sum.rtf", 
          file.path(deploy_dir, "data", "rt-dm-sum.rtf"))

# Create modified app.R that sources local files
app_content <- readLines("inst/shiny-app/app.R")

# Add sourcing after library calls
library_end <- max(which(grepl("^library\\(", app_content)))

source_code <- c(
  "",
  "# Source all stateful functions",
  "source_files <- list.files('R', pattern = '\\\\.R$', full.names = TRUE)",
  "for (file in source_files) {",
  "  source(file)",
  "}",
  "",
  "# Initialize stateful patterns",
  "if (exists('.onLoad')) .onLoad()",
  ""
)

# Insert source code
app_content <- c(
  app_content[1:library_end],
  source_code,
  app_content[(library_end + 1):length(app_content)]
)

# Remove all stateful:: prefixes
app_content <- gsub("stateful::", "", app_content)

# Fix sample file path
app_content <- gsub('system.file\\("extdata", package = "stateful"\\)', 
                    '"data"', 
                    app_content)

# Write the standalone app
writeLines(app_content, file.path(deploy_dir, "app.R"))

cat("Created standalone app in:", deploy_dir, "\n")
cat("You can now deploy using:\n")
cat("rsconnect::deployApp('", deploy_dir, "', appName = 'stateful', account = 'tarapk', server = 'davinci-platform-exp.web.bms.com')\n", sep = "")