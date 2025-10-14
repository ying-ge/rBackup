#' @keywords internal
"_PACKAGE"
 
## usethis namespace: start
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom httr GET add_headers status_code content
#' @importFrom curl curl_download
#' @importFrom tools write_PACKAGES
#' @importFrom utils download.file available.packages install.packages
#' @importFrom digest digest
## usethis namespace: end
NULL
 
#' rBackup: Comprehensive R Package Backup and Offline Management System
#'
#' @description
#' The rBackup package provides a comprehensive backup and offline management 
#' system for R packages from multiple sources (GitHub, CRAN, Bioconductor). 
#' Unlike installation-only tools like pak and remotes, rBackup focuses on 
#' creating reliable offline archives and automated backup workflows.
#'
#' @section Main functions:
#' \itemize{
#'   \item \code{\link{backup_github_packages}}: Backup GitHub packages
#'   \item \code{\link{backup_cran_packages}}: Backup CRAN/Bioconductor packages
#'   \item \code{\link{install_from_backup}}: Install from backups
#'   \item \code{\link{create_local_repo}}: Create local package repository
#'   \item \code{\link{generate_backup_workflow}}: Generate automated workflows
#' }
#'
#' @section Getting started:
#' See `vignette("getting-started", package = "rBackup")` for an introduction.
#'
"_PACKAGE"
