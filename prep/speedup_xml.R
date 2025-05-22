library(xml2)
library(tictoc)

tic()
dt <- xml2::read_xml("https://monitor.statnipokladna.gov.cz/data/polvyk.xml")
toc()

nms <- dt %>% xml_children() %>% purrr::pluck(1) %>% purrr::map(function(x) x %>% xml2::xml_children() %>% xml2::xml_name())

microbenchmark::microbenchmark(
  {
    x <- dt %>%
      xml_children() %>%
      xml_children() %>%
      xml_text() %>%
      matrix(nrow = 8) %>% t() %>% as.data.frame()

  },
  {
    y <- dt %>%
      xml_children() %>%
      as_list()

  }, times = 3
)


tic()
x <- dt %>%
  xml_children() %>%
  purrr::map_dfr(function(x) {x %>% xml2::xml_children() %>%
      xml2::xml_text() %>%
      as.character() %>%
      t() %>%
      tibble::as_tibble() %>%
      invisible()})
toc()

microbenchmark::microbenchmark(
  {x <- dt %>%
    xml_children() %>%
    purrr::map_dfr(~xml2::xml_children(.) %>%
                     xml2::xml_text() %>%
                     as.character() %>%
                     t() %>%
                     tibble::as_tibble() %>%
                     invisible())},
  {x <- dt %>%
    xml_children() %>%
    purrr::map_dfr(function(x) {x %>% xml2::xml_children() %>%
        xml2::xml_text() %>%
        as.character() %>%
        t() %>%
        tibble::as_tibble() %>%
        invisible()})}, times = 10
)
