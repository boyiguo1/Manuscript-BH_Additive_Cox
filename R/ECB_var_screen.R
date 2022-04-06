#' Use GAM to screen the features
#'
#' @param dat the raw data
#' @param nrow How many rows are selected
#'
#' @return
#'
var_screen <- function(dat, nrow = 200){
  dat %>%
    select(starts_with("mz")) %>%
    apply(,MARGIN = 2, FUN = var) %>%
    sort(decreasing = TRUE) %>%
    head(nrow) %>%
    names
  # browser()
  # names() %>%
  # map_dfr(#feature_name[1:2],
  #   .f = function(name, .dat){
  #     # name <- feature_name[1]
  #     y <- .dat %>% pull(death3yr)
  #     x <- .dat %>% pull({{name}})
  #     mgcv::gam(y ~ s(x, bs = "cr", k = 10), family = binomial(),
  #               data = data.frame(x, y)) %>%
  #       tidy() %>%
  #       filter(term == "s(x)") %>%
  #       select(p.value) %>%
  #       mutate(var = name,
  #              p.value)
  #
  #   },
  #   .dat = dat) %>% data.frame(
  #     .,
  #     p.adj = p.adjust(.$p.value, "fdr")
  #   ) %>% arrange(p.adj) %>% #filter(p.adj<0.2)
  # head(nrow)
}
