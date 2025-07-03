library(tidyverse)
library(reactable)

# load your prototype project environment
# or re-source the qmd chunks if needed

# Assuming the tables are generated like this:
comm_summary_table_1 <- read_csv("data/comm_summary_table_1.csv")  # or generate
comm_summary_table_2 <- read_csv("data/comm_summary_table_2.csv")  # or generate

saveRDS(comm_summary_table_1, "rds_objects/comm_summary_table_1.rds")
saveRDS(comm_summary_table_2, "rds_objects/comm_summary_table_2.rds")

