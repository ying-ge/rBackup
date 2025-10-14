#' List Available Backups
#'
#' List all available package backups in the specified directory.
#'
#' @param backup_dir Backup directory to scan
#' @param type Type of backup ("github", "cran", or "all")
#'
#' @return Data frame with backup information
#'
#' @examples
#' \dontrun{
#' # List all GitHub backups
#' github_backups <- list_backups("github_backup", type = "github")
#' 
#' # List all backups
#' all_backups <- list_backups(type = "all")
#' }
#'
#' @export
list_backups <- function(backup_dir = ".", type = c("github", "cran", "all")) {
  type <- match.arg(type)
  
  if (type == "all") {
    github_backups <- list_backups("github_backup", "github")
    cran_backups <- list_backups("cran_backup", "cran")
    return(rbind(github_backups, cran_backups))
  }
  
  if (!dir.exists(backup_dir)) {
    warning("Backup directory does not exist: ", backup_dir)
    return(data.frame())
  }
  
  # Find log files
  log_files <- list.files(backup_dir, pattern = "backup_log.*\\.csv$", full.names = TRUE)
  
  if (length(log_files) == 0) {
    warning("No backup log files found in: ", backup_dir)
    return(data.frame())
  }
  
  # Read the most recent log
  latest_log <- log_files[which.max(file.mtime(log_files))]
  backup_data <- read.csv(latest_log, stringsAsFactors = FALSE)
  
  # Add backup type
  backup_data$backup_type <- type
  
  return(backup_data)
}
 
#' Verify Backup Integrity
#'
#' Check the integrity of backup files and report any issues.
#'
#' @param backup_dir Directory containing backups
#' @param fix_issues Logical, whether to attempt fixing issues
#'
#' @return List with verification results
#'
#' @examples
#' \dontrun{
#' # Verify GitHub backups
#' verification <- verify_backup_integrity("github_backup")
#' print(verification$summary)
#' }
#'
#' @export
verify_backup_integrity <- function(backup_dir, fix_issues = FALSE) {
  if (!dir.exists(backup_dir)) {
    stop("Backup directory does not exist: ", backup_dir)
  }
  
  results <- list(
    verified_packages = character(),
    corrupted_packages = character(),
    missing_files = character(),
    summary = list()
  )
  
  # Get list of package directories
  pkg_dirs <- list.dirs(backup_dir, recursive = FALSE, full.names = TRUE)
  pkg_dirs <- pkg_dirs[basename(pkg_dirs) != ""]  # Remove empty names
  
  for (pkg_dir in pkg_dirs) {
    pkg_name <- basename(pkg_dir)
    
    # Check for expected files
    zip_files <- list.files(pkg_dir, pattern = "\\.(zip|tar\\.gz)$")
    
    if (length(zip_files) == 0) {
      results$missing_files <- c(results$missing_files, pkg_name)
      next
    }
    
    # Verify file integrity
    main_file <- file.path(pkg_dir, zip_files[1])
    
    if (file.size(main_file) < 1000) {  # Suspiciously small
      results$corrupted_packages <- c(results$corrupted_packages, pkg_name)
      
      if (fix_issues) {
        unlink(pkg_dir, recursive = TRUE)
      }
    } else {
      results$verified_packages <- c(results$verified_packages, pkg_name)
    }
  }
  
  # Create summary
  results$summary <- list(
    total_packages = length(pkg_dirs),
    verified = length(results$verified_packages),
    corrupted = length(results$corrupted_packages),
    missing_files = length(results$missing_files)
  )
  
  return(results)
}
 
#' Clean Old Backups
#'
#' Remove backup files older than specified age to save disk space.
#'
#' @param backup_dir Directory containing backups
#' @param older_than Age threshold (e.g., "30 days", "6 months")
#' @param dry_run Logical, if TRUE, only show what would be deleted
#'
#' @return Character vector of cleaned files/directories
#'
#' @examples
#' \dontrun{
#' # Show what would be cleaned (dry run)
#' to_clean <- clean_old_backups("github_backup", "60 days", dry_run = TRUE)
#' 
#' # Actually clean old backups
#' cleaned <- clean_old_backups("github_backup", "60 days", dry_run = FALSE)
#' }
#'
#' @export
clean_old_backups <- function(backup_dir, older_than = "30 days", dry_run = TRUE) {
  if (!dir.exists(backup_dir)) {
    stop("Backup directory does not exist: ", backup_dir)
  }
  
  # Parse age threshold
  age_seconds <- parse_age_threshold(older_than)
  cutoff_time <- Sys.time() - age_seconds
  
  # Find old files
  all_files <- list.files(backup_dir, recursive = TRUE, full.names = TRUE)
  old_files <- character()
  
  for (file in all_files) {
    if (file.mtime(file) < cutoff_time) {
      old_files <- c(old_files, file)
    }
  }
  
  if (length(old_files) == 0) {
    message("No old backup files found")
    return(character())
  }
  
  if (dry_run) {
    message("Would delete ", length(old_files), " old files:")
    for (file in old_files) {
      message("  ", file)
    }
    return(old_files)
  } else {
    message("Deleting ", length(old_files), " old files...")
    for (file in old_files) {
      if (file.exists(file)) {
        unlink(file)
      }
    }
    
    # Clean empty directories
    clean_empty_dirs(backup_dir)
    
    return(old_files)
  }
}
 
#' @keywords internal
parse_age_threshold <- function(age_string) {
  # Simple parser for age strings like "30 days", "6 months", etc.
  parts <- strsplit(tolower(age_string), " ")[[1]]
  
  if (length(parts) != 2) {
    stop("Invalid age format. Use format like '30 days' or '6 months'")
  }
  
  number <- as.numeric(parts[1])
  unit <- parts[2]
  
  multiplier <- switch(unit,
                      "day" = , "days" = 24 * 60 * 60,
                      "week" = , "weeks" = 7 * 24 * 60 * 60,
                      "month" = , "months" = 30 * 24 * 60 * 60,
                      "year" = , "years" = 365 * 24 * 60 * 60,
                      stop("Unknown time unit: ", unit))
  
  return(number * multiplier)
}
 
#' @keywords internal
clean_empty_dirs <- function(path) {
  dirs <- list.dirs(path, recursive = TRUE, full.names = TRUE)
  
  for (dir in rev(dirs)) {  # Process in reverse order (deepest first)
    if (length(list.files(dir, recursive = TRUE)) == 0 && dir != path) {
      unlink(dir, recursive = TRUE)
    }
  }
}
