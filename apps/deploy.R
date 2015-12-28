

# Install dependencies ----------------------------------------------------

devtools::install_github("rappster/timetrackr")

# Copy dev to productive --------------------------------------------------

name <- "timetrackr"
vsn <- "v0.2.1"

vsn_name <- sprintf("%s_%s", name, vsn)
from <- file.path("apps", vsn_name)
to <- "apps/production"

prod_dir <- file.path(to, name)

dir.create(to, showWarnings = FALSE)

# files <- list.files(from, recursive = TRUE, full.names = TRUE)
# sapply(files, file.copy, to = to, overwrite = TRUE)

file.copy(from = from, to = to, recursive = TRUE, overwrite = TRUE)

unlink(prod_dir, recursive = TRUE, force = TRUE)
file.rename(from = file.path(to, vsn_name), to = prod_dir)

# Deploy ------------------------------------------------------------------

rsconnect::deployApp(prod_dir, account = "rappster")
