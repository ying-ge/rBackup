#' Backup GitHub R Packages
#'
#' Download and backup R packages from GitHub repositories, creating a local
#' archive for offline installation.
#'
#' @param repo_list Character vector of GitHub repositories in "user/repo" format
#' @param backup_dir Directory to store backups (default: "github_backup")
#' @param max_size_mb Maximum file size in MB (larger files will be skipped)
#' @param include_info Logical, whether to download repository information
#' @param progress Logical, whether to show progress messages
#'
#' @return A data frame with backup results including status, file size, and metadata
#'
#' @examples
#' \dontrun{
#' # Backup specific GitHub packages
#' repos <- c("tidyverse/ggplot2", "rstudio/shiny")
#' result <- backup_github_packages(repos, backup_dir = "my_backups")
#' 
#' # View results
#' print(result)
#' }
#'
#' @export
backup_github_packages <- function(repo_list, 
                                  backup_dir = "github_backup",
                                  max_size_mb = 100,
                                  include_info = TRUE,
                                  progress = TRUE) {
  
  if (length(repo_list) == 0) {
    stop("repo_list cannot be empty")
  }
  
  # Validate repo format
  invalid_repos <- !grepl("^[^/]+/[^/]+$", repo_list)
  if (any(invalid_repos)) {
    stop("Invalid repository format. Use 'user/repo' format.")
  }
  
  # Create backup directory
  if (!dir.exists(backup_dir)) {
    dir.create(backup_dir, recursive = TRUE)
  }
  
  # Initialize results
  results <- data.frame(
    repo = character(),
    status = character(),
    file_size_mb = numeric(),
    download_date = character(),
    branch = character(),
    stringsAsFactors = FALSE
  )
  
  # Get GitHub token if available
  github_token <- get_github_token()
  
  for (i in seq_along(repo_list)) {
    repo <- repo_list[i]
    
    if (progress) {
      message(sprintf("[%d/%d] Processing: %s", i, length(repo_list), repo))
    }
    
    result <- backup_single_github_repo(
      repo = repo,
      backup_dir = backup_dir,
      max_size_mb = max_size_mb,
      github_token = github_token,
      include_info = include_info
    )
    
    results <- rbind(results, result)
    
    # Save progress
    saveRDS(results, file.path(backup_dir, "backup_progress.rds"))
    
    # Rate limiting
    if (i %% 10 == 0) {
      Sys.sleep(1)
    }
  }
  
  # Save final results
  log_file <- file.path(backup_dir, paste0("backup_log_", Sys.Date(), ".csv"))
  write.csv(results, log_file, row.names = FALSE)
  
  if (progress) {
    print_backup_summary(results)
  }
  
  invisible(results)
}
 
#' @keywords internal
backup_single_github_repo <- function(repo, backup_dir, max_size_mb, 
                                     github_token, include_info) {
  repo_name <- gsub("/", "_", repo)
  repo_dir <- file.path(backup_dir, repo_name)
  
  # Clean existing directory
  if (dir.exists(repo_dir)) {
    unlink(repo_dir, recursive = TRUE)
  }
  dir.create(repo_dir)
  
  result <- data.frame(
    repo = repo,
    status = "failed",
    file_size_mb = 0,
    download_date = as.character(Sys.Date()),
    branch = "",
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    # Try different branches
    branches <- c("main", "master")
    downloaded <- FALSE
    
    for (branch in branches) {
      zip_url <- sprintf("https://github.com/%s/archive/refs/heads/%s.zip", 
                        repo, branch)
      zip_file <- file.path(repo_dir, sprintf("%s_%s.zip", repo_name, branch))
      
      tryCatch({
        download.file(zip_url, zip_file, mode = "wb", quiet = TRUE)
        
        if (file.exists(zip_file) && file.size(zip_file) > 1000) {
          file_size_mb <- file.size(zip_file) / (1024^2)
          
          if (file_size_mb > max_size_mb) {
            result$status <- sprintf("skipped_large_%.1fMB", file_size_mb)
            result$file_size_mb <- file_size_mb
            file.remove(zip_file)
            break
          }
          
          result$status <- "success"
          result$file_size_mb <- round(file_size_mb, 2)
          result$branch <- branch
          downloaded <- TRUE
          
          # Download repo info if requested
          if (include_info && nchar(github_token) > 0) {
            download_repo_info(repo, repo_dir, github_token)
          }
          
          break
        } else {
          if (file.exists(zip_file)) file.remove(zip_file)
        }
      }, error = function(e) {
        if (file.exists(zip_file)) file.remove(zip_file)
      })
    }
    
    if (!downloaded && result$status == "failed") {
      unlink(repo_dir, recursive = TRUE)
    }
    
  }, error = function(e) {
    result$status <<- paste("error:", substr(e$message, 1, 50))
    unlink(repo_dir, recursive = TRUE)
  })
  
  return(result)
}
 
#' @keywords internal
get_github_token <- function() {
  token <- Sys.getenv("GITHUB_PAT")
  if (token == "") {
    token <- Sys.getenv("GITHUB_TOKEN")
  }
  return(token)
}
 
#' @keywords internal
download_repo_info <- function(repo, repo_dir, github_token) {
  api_url <- sprintf("https://api.github.com/repos/%s", repo)
  
  tryCatch({
    if (nchar(github_token) > 0) {
      response <- httr::GET(api_url, httr::add_headers(
        Authorization = paste("token", github_token)
      ))
    } else {
      response <- httr::GET(api_url)
    }
    
    if (httr::status_code(response) == 200) {
      repo_info <- httr::content(response, "text", encoding = "UTF-8")
      writeLines(repo_info, file.path(repo_dir, "repository_info.json"))
    }
  }, error = function(e) {
    # Silently fail if repo info can't be downloaded
  })
}
 
#' @keywords internal
print_backup_summary <- function(results) {
  success_count <- sum(results$status == "success")
  skipped_count <- sum(grepl("skipped_large", results$status))
  failed_count <- nrow(results) - success_count - skipped_count
  total_size <- sum(results$file_size_mb[results$status == "success"])
  
  message("\n=== Backup Summary ===")
  message("Success: ", success_count)
  message("Skipped (large): ", skipped_count)
  message("Failed: ", failed_count)
  message("Total size: ", round(total_size, 1), " MB")
}
