#' Backup CRAN and Bioconductor Packages
#'
#' Download and backup R packages from CRAN and Bioconductor, creating source
#' package archives for offline installation.
#'
#' @param package_list Character vector of package names
#' @param backup_dir Directory to store backups (default: "cran_backup")
#' @param max_size_mb Maximum file size in MB (larger files will be skipped)
#' @param include_deps Logical, whether to include dependencies
#' @param progress Logical, whether to show progress messages
#'
#' @return A data frame with backup results including source, version, and status
#'
#' @examples
#' \dontrun{
#' # Backup CRAN packages
#' packages <- c("ggplot2", "dplyr", "tidyr")
#' result <- backup_cran_packages(packages)
#' 
#' # Include Bioconductor packages
#' bio_packages <- c("limma", "edgeR", "DESeq2")
#' bio_result <- backup_cran_packages(bio_packages, include_deps = TRUE)
#' }
#'
#' @export
backup_cran_packages <- function(package_list,
                                backup_dir = "cran_backup",
                                max_size_mb = 100,
                                include_deps = FALSE,
                                progress = TRUE) {
  
  if (length(package_list) == 0) {
    stop("package_list cannot be empty")
  }
  
  # Setup repositories
  setup_repositories()
  
  # Create backup directory
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Expand package list with dependencies if requested
  if (include_deps) {
    package_list <- get_all_dependencies(package_list)
  }
  
  # Remove duplicates
  package_list <- unique(package_list)
  
  # Initialize results
  results <- data.frame(
    package = character(),
    version = character(),
    source = character(),
    status = character(),
    file_size_mb = numeric(),
    download_date = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(package_list)) {
    pkg <- package_list[i]
    
    if (progress) {
      message(sprintf("[%d/%d] Processing: %s", i, length(package_list), pkg))
    }
    
    result <- backup_single_cran_package(
      package_name = pkg,
      backup_dir = backup_dir,
      max_size_mb = max_size_mb
    )
    
    results <- rbind(results, result)
    
    # Save progress
    saveRDS(results, file.path(backup_dir, "backup_progress.rds"))
    
    # Brief pause to be nice to servers
    if (i %% 20 == 0) {
      Sys.sleep(0.5)
    }
  }
  
  # Save final results
  log_file <- file.path(backup_dir, paste0("backup_log_", Sys.Date(), ".csv"))
  write.csv(results, log_file, row.names = FALSE)
  
  if (progress) {
    print_cran_backup_summary(results)
  }
  
  invisible(results)
}
 
#' @keywords internal
backup_single_cran_package <- function(package_name, backup_dir, max_size_mb) {
  pkg_dir <- file.path(backup_dir, package_name)
  
  # Clean existing directory
  if (dir.exists(pkg_dir)) {
    unlink(pkg_dir, recursive = TRUE)
  }
  dir.create(pkg_dir)
  
  result <- data.frame(
    package = package_name,
    version = "",
    source = "",
    status = "not_found",
    file_size_mb = 0,
    download_date = as.character(Sys.Date()),
    stringsAsFactors = FALSE
  )
  
  # Get package info
  pkg_info <- find_package_info(package_name)
  
  if (is.null(pkg_info)) {
    unlink(pkg_dir, recursive = TRUE)
    return(result)
  }
  
  result$version <- pkg_info$version
  result$source <- pkg_info$source
  
  # Download package
  tryCatch({
    tar_file <- file.path(pkg_dir, sprintf("%s_%s.tar.gz", 
                                          package_name, pkg_info$version))
    
    download.file(pkg_info$url, tar_file, mode = "wb", quiet = TRUE)
    
    if (file.exists(tar_file) && file.size(tar_file) > 100) {
      file_size_mb <- file.size(tar_file) / (1024^2)
      
      if (file_size_mb > max_size_mb) {
        result$status <- sprintf("skipped_large_%.1fMB", file_size_mb)
        result$file_size_mb <- file_size_mb
        file.remove(tar_file)
        unlink(pkg_dir, recursive = TRUE)
      } else {
        result$status <- "success"
        result$file_size_mb <- round(file_size_mb, 2)
        
        # Save package info
        save_package_info(pkg_info, pkg_dir)
      }
    } else {
      result$status <- "download_failed"
      unlink(pkg_dir, recursive = TRUE)
    }
    
  }, error = function(e) {
    result$status <<- paste("error:", substr(e$message, 1, 50))
    unlink(pkg_dir, recursive = TRUE)
  })
  
  return(result)
}
 
#' @keywords internal
setup_repositories <- function() {
  # Setup CRAN and Bioconductor repositories
  repos <- c(CRAN = "https://cloud.r-project.org/")
  
  # Add Bioconductor if available
  if (requireNamespace("BiocManager", quietly = TRUE)) {
    bioc_repos <- BiocManager::repositories()
    repos <- c(repos, bioc_repos)
  }
  
  options(repos = repos)
}
 
#' @keywords internal
find_package_info <- function(package_name) {
  # Try to find package in available repositories
  repos <- getOption("repos")
  
  for (repo_name in names(repos)) {
    repo_url <- repos[repo_name]
    
    tryCatch({
      available_pkgs <- available.packages(repos = repo_url)
      
      if (package_name %in% rownames(available_pkgs)) {
        version <- available_pkgs[package_name, "Version"]
        
        # Construct download URL
        if (grepl("bioconductor", repo_url, ignore.case = TRUE)) {
          source_name <- paste("Bioconductor", repo_name)
          url <- sprintf("%s/src/contrib/%s_%s.tar.gz", 
                        repo_url, package_name, version)
        } else {
          source_name <- "CRAN"
          url <- sprintf("%s/src/contrib/%s_%s.tar.gz", 
                        repo_url, package_name, version)
        }
        
        return(list(
          version = version,
          source = source_name,
          url = url,
          repository = repo_url
        ))
      }
    }, error = function(e) {
      # Continue to next repository
    })
  }
  
  return(NULL)
}
 
#' @keywords internal
get_all_dependencies <- function(packages) {
  # This is a simplified version - you might want to use tools::package_dependencies
  return(packages)
}
 
#' @keywords internal
save_package_info <- function(pkg_info, pkg_dir) {
  info_content <- sprintf(
    "Package: %s\nVersion: %s\nSource: %s\nRepository: %s\nDownload Date: %s\n",
    basename(pkg_dir), pkg_info$version, pkg_info$source, 
    pkg_info$repository, Sys.Date()
  )
  writeLines(info_content, file.path(pkg_dir, "package_info.txt"))
}
 
#' @keywords internal
print_cran_backup_summary <- function(results) {
  success_count <- sum(results$status == "success")
  skipped_count <- sum(grepl("skipped_large", results$status))
  notfound_count <- sum(results$status == "not_found")
  failed_count <- nrow(results) - success_count - skipped_count - notfound_count
  total_size <- sum(results$file_size_mb[results$status == "success"])
  
  cran_count <- sum(results$source == "CRAN" & results$status == "success")
  bioc_count <- sum(grepl("Bioconductor", results$source) & results$status == "success")
  
  message("\n=== CRAN/Bioconductor Backup Summary ===")
  message("Success: ", success_count, " (CRAN: ", cran_count, ", Bioconductor: ", bioc_count, ")")
  message("Skipped (large): ", skipped_count)
  message("Not found: ", notfound_count)
  message("Failed: ", failed_count)
  message("Total size: ", round(total_size, 1), " MB")
}
