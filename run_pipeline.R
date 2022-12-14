# This script is useful for kicking off the pipeline
# with an RStudio background job, so that your main
# console isn't bogged down as it runs.

library(targets)
tar_make()
