
# Meta --------------------------------------------------------------------

name <- devtools::as.package(".")$package
vsn <- sprintf("v%s", devtools::as.package(".")$version)
vsn_name <- sprintf("%s_%s", name, vsn)
git_branch <- sprintf("release_%s", vsn)

from <- file.path("apps", vsn_name)
to <- "apps/production"

prod_dir <- file.path(to, name)

# Copy dev to productive --------------------------------------------------

## Ensure that it's empty //
unlink(prod_dir, recursive = TRUE, force = TRUE)
dir.create(prod_dir, recursive = TRUE, showWarnings = FALSE)

# files <- list.files(from, recursive = TRUE, full.names = TRUE)
# sapply(files, file.copy, to = to, overwrite = TRUE)

file.copy(from = from, to = to, recursive = TRUE, overwrite = TRUE)

unlink(prod_dir, recursive = TRUE, force = TRUE)
file.rename(from = file.path(to, vsn_name), to = prod_dir)
