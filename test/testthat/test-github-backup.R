test_that("GitHub backup validation works", {
  # Test repo format validation
  expect_error(backup_github_packages("invalid-repo-format"))
  expect_error(backup_github_packages(""))
  expect_error(backup_github_packages(character(0)))
  
  # Test valid format
  expect_silent(backup_github_packages("user/repo", backup_dir = tempdir()))
})
 
test_that("backup directory creation works", {
  temp_dir <- file.path(tempdir(), "test_backup")
  
  # Should create directory if it doesn't exist
  result <- backup_github_packages("hadley/devtools", 
                                  backup_dir = temp_dir,
                                  max_size_mb = 1)  # Small limit to avoid actual download
  
  expect_true(dir.exists(temp_dir))
  
  # Cleanup
  unlink(temp_dir, recursive = TRUE)
})
