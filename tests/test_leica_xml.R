# Check the Leica XML reader on one exported XML (e.g. Project/MetaData/Image002.xml)
# or a whole .lif.
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# Run from the pipeline folder:
#   source("tests/test_leica_xml.R"); test_leica_xml("/path/to/MetaData/Image002.xml")
#   test_leica_xml("/path/to/Project.lif")
source("R/utils.R"); source("R/leica_xml.R")

test_leica_xml <- function(path) {
  require_pkgs("xml2")
  if (grepl("\\.lif$", path, ignore.case = TRUE)) {
    doc <- read_lif_xml(path)
    li <- lif_image_table(doc)
    cat(nrow(li$table), "images found in the .lif XML. First few paths:\n")
    print(utils::head(li$table$path, 10))
    node <- xml2::xml_find_first(li$nodes[[li$table$node_index[1]]], "./Data/Image")
  } else {
    node <- xml2::xml_find_first(xml2::read_xml(path), "//Image")
  }
  p <- parse_leica_image(node)
  print(t(p$image))
  print(p$channels)
  invisible(p)
}
# Expected for Image002.xml: 20x/0.75 DRY, zoom 1, 4 sequential settings,
# C1 DAPI 405nm@10.2%, C2 ALEXA 488 488nm@10%, C3 ALEXA 568 561nm@10%, C4 ALEXA 647 638nm@10%
