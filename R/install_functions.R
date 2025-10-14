#' Install Package from Backup
#'
#' Install R packages from local backups created by rBackup.
#'
#' @param package_name Name of the package or repository (for GitHub: "user/repo")
#' @param backup_dir Directory containing the backups
#' @param type Type of backup ("auto", "github", "cran")
#' @param dependencies Logical, whether to install dependencies
#'
#' @return Logical indicating success
#'
#' @examples
#' \dontrun{
#' # Install from GitHub backup
#' install_from_backup("tidyverse/ggplot2", type = "github")
#' 
#' # Install from CRAN backup
#' install_from_backup("ggplot2", type = "cran")
#' 
#' # Auto-detect type
#' install_from_backup("ggplot2")
#' }
#'
#' @export
install_from_backup <- function(package_name, 
                               backup_dir = NULL,
                               type = c("auto", "github", "cran"),
                               dependencies = TRUE) {
  
  type <- match.arg(type)
  
  # Auto-detect backup directory and type if not specified
  if (is.null(backup_dir)) {
    if (type == "auto") {
      type <- detect_backup_type(package_name)
    }
    backup_dir <- get_default_backup_dir(type)
  }
  
  # Validate backup directory exists
  if (!dir.exists(backup_dir)) {
    stop("Backup directory does not exist: ", backup_dir)
  }
  
  # Dispatch to appropriate installer
  switch(type,
         "github" = install_from_github_backup(package_name, backup_dir, dependencies),
         "cran" = install_from_cran_backup(package_name, backup_dir, dependencies),
         stop("Unknown backup type: ", type)
  )
}
 
#' @keywords internal
install_from_github_backup <- function(repo_name, backup_dir, dependencies) {
  repo_dir_name <- gsub("/", "_", repo_name)
  repo_path <- file.path(backup_dir, repo_dir_name)
  
  if (!dir.exists(repo_path)) {
    stop("GitHub backup not found for: ", repo_name)
  }
  
  # Find zip file
  zip_files <- list.files(repo_path, pattern = "\\.zip$", full.names = TRUE)
  if (length(zip_files) == 0) {
    stop("No zip files found in backup for: ", repo_name)
  }
  
  zip_file <- zip_files[1]
  message("Installing ", repo_name, " from ", basename(zip_file))
  
  # Extract to temporary directory
  temp_dir <- tempdir()
  extract_dir <- file.path(temp_dir, paste0("rBackup_", Sys.getpid()))
  dir.create(extract_dir)
  
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
  
  tryCatch({
    unzip(zip_file, exdir = extract_dir)
    
    # Find the package directory
    extracted_dirs <- list.dirs(extract_dir, recursive = FALSE)
    if (length(extracted_dirs) == 0) {
      stop("Failed to extract package from zip file")
    }
    
    pkg_dir <- extracted_dirs[1]
    
    # Check if it's a valid R package
    desc_file <- file.path(pkg_dir, "DESCRIPTION")
    if (!file.exists(desc_file)) {
      stop("Not a valid R package (no DESCRIPTION file found)")
    }
    
    # Install the package
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop("devtools package is required for installing from GitHub backups")
    }
    
    devtools::install(pkg_dir, dependencies = dependencies)
    
    message("Successfully installed ", repo_name)
    return(TRUE)
    
  }, error = function(e) {
    stop("Failed to install ", repo_name, ": ", e$message)
  })
}
 
#' @keywords internal
install_from_cran_backup <- function(package_name, backup_dir, dependencies) {
  pkg_path <- file.path(backup_dir, package_name)
  
  if (!dir.exists(pkg_path)) {
    stop("CRAN backup not found for: ", package_name)
  }
  
  # Find tar.gz file
  tar_files <- list.files(pkg_path, pattern = "\\.tar\\.gz$", full.names = TRUE)
  if (length(tar_files) == 0) {
    stop("No tar.gz files found in backup for: ", package_name)
  }
  
  tar_file <- tar_files[1]
  message("Installing ", package_name, " from ", basename(tar_file))
  
  tryCatch({
    install.packages(tar_file, repos = NULL, type = "source", 
                    dependencies = dependencies)
    
    message("Successfully installed ", package_name)
    return(TRUE)
    
  }, error = function(e) {
    stop("Failed to install ", package_name, ": ", e$message)
  })
}
 
#' @keywords internal
detect_backup_type <- function(package_name) {
  # Simple heuristic: if contains "/", likely GitHub
  if (grepl("/", package_name)) {
    return("github")
  } else {
    return("cran")
  }
}
 
#' @keywords internal
get_default_backup_dir <- function(type) {
  switch(type,
         "github" = "github_backup",
         "cran" = "cran_backup",
         ".")
}
 
#' Batch Install from Backups
#'
#' Install multiple packages from backups in one operation.
#'
#' @param package_list Character vector of package names/repositories
#' @param backup_dirs Named list of backup directories, or single directory
#' @param continue_on_error Logical, whether to continue if one package fails
#' @param progress Logical, whether to show progress
#'
#' @return Data frame with installation results
#'
#' @examples
#' \dontrun{
#' # Install multiple packages
#' packages <- c("ggplot2", "dplyr", "tidyverse/stringr")
#' results <- batch_install_from_backup(packages)
#' }
#'
#' @export
batch_install_from_backup <- function(package_list,
                                     backup_dirs = NULL,
                                     continue_on_error = TRUE,
                                     progress = TRUE) {
  
  results <- data.frame(
    package = character(),
    status = character(),
    error_message = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(package_list)) {
    pkg <- package_list[i]
    
    if (progress) {
      message(sprintf("[%d/%d] Installing: %s", i, length(package_list), pkg))
    }
    
    result <- data.frame(
      package = pkg,
      status = "failed",
      error_message = "",
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      install_from_backup(pkg, backup_dir = backup_dirs)
      result$status <- "success"
    }, error = function(e) {
      result$error_message <<- e$message
      if (!continue_on_error) {
        stop("Installation failed for ", pkg, ": ", e$message)
      }
    })
    
    results <- rbind(results, result)
  }
  
  if (progress) {
    success_count <- sum(results$status == "success")
    message("\nBatch installation complete: ", success_count, "/", 
            nrow(results), " packages installed successfully")
  }
  
  return(results)
}
