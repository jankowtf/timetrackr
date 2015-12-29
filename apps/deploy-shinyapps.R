
# Meta --------------------------------------------------------------------

name <- devtools::as.package(".")$package
vsn <- sprintf("v%s", devtools::as.package(".")$version)
vsn_name <- sprintf("%s_%s", name, vsn)
git_branch <- sprintf("release_%s", vsn)

from <- file.path("apps", vsn_name)
to <- "apps/production"

prod_dir <- file.path(to, name)

# Install dependencies ----------------------------------------------------

## Ensure clean install //
remove.packages(name)
devtools::install_github("rappster/timetrackr", ref = git_branch)

# Deploy ------------------------------------------------------------------

rsconnect::deployApp(prod_dir, account = "rappster")
