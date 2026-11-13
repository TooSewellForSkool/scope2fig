# Check the name pre-fill patterns on example names. Run from the pipeline folder:
#   source("tests/test_name_patterns.R")
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
source("R/utils.R")
fields <- c("experimental_id", "animal_replicate", "section_coverslip", "fov", "notes")
names <- c("(+)BAF_DIV1-DAPI_Coro1a(488)_Map2(568)_TH(647)-20X-02-ApoTome RAW Convert-39",
           "(-)BAF_DIV7-DAPI_Coro1a(488)_Map2(568)_TH(647)-20X-ApoTome RAW Convert-30",
           "SLIDE3-2/Series004", "Series002", "KO-mouse3-hippocampus-FoV2")
p <- parse_name_fields(names, default_name_patterns(), fields)
print(cbind(name = substr(names, 1, 40), p[, 1:4]))
stopifnot(
  p$experimental_id[1] == "(+)BAF DIV1", p$fov[1] == "02",
  p$experimental_id[2] == "(-)BAF DIV7", is.na(p$fov[2]),
  p$section_coverslip[3] == "SLIDE3-2",
  all(is.na(unlist(p[4, ]))),                       # default LAS X name: nothing invented
  p$experimental_id[5] == "KO", p$animal_replicate[5] == "mouse3", p$fov[5] == "FoV2")
cat("All name-pattern tests passed.\n")
