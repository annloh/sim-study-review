library(wosr)
library(tidyverse)


# Define Character Vector with Journal names ------------------------------

journal <- c(
  "STATISTICS IN MEDICINE",
  "BMC MEDICAL RESEARCH METHODOLOGY",
  "STATISTICAL METHODS IN MEDICAL RESEARCH",
  "MULTIVARIATE BEHAVIORAL RESEARCH",
  "PSYCHOLOGICAL METHODS"
)


# Define vector with years ------------------------------------------------

year <- seq(2000, 2018, by = 2)

# Combine all Years and Jounals -------------------------------------------

query_data <- expand.grid(journal = journal,
                          year = year,
                          stringsAsFactors = FALSE)

query_data <- cbind(id = 1:nrow(query_data), query_data)


query <- function(journal, year) {
  paste0("SO = (", journal, ") AND PY = (", year, ")")
}
# Session id can be obtained from the url when logged in with the leiden proxy
session_id <- "E5zjIn18ZLYdVeta6Ml"


pull_refs <-
  query_data %>% pmap(function(journal, year, id) {
    list(
      id = id,
      refs = pull_wos(
        query = query(journal, year),
        edition = c("SSCI"),
        sid = session_id
      )$publication
    )
  })

refs_df <- tibble(id = map(pull_refs, ~ {.x$id}),
  refs = map(pull_refs, ~ {
  .x$refs
  })) %>%
    unnest(cols = c(id, refs))

refs_sorted <- refs_df %>% mutate(id = as.character(id)) %>%
  group_by(id) %>%
  arrange(id, desc(tot_cites))

# refs_sorted %>% ungroup() %>%
#   filter(id == 1) %>%
#   select(abstract) %>%
#   head(n=1) %>%
#   as.character()
