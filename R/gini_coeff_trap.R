

gini_coeff_trap <- function(score_data) {

  names(score_data <- c("score", "bad"))

  tot_area <- score_data %>%
    mutate(good = 1-bad) %>%
    group_by(score) %>%
    summarise(bad = sum(bad),
              good = sum(good)) %>%
    mutate(cumsum_bad = cumsum(bad)/sum(bad),
           cumsum_good = cumsum(good)/sum(good),
           bad_inc = c(cumsum_bad[1],diff(cumsum_bad)),
           good_inc = c(cumsum_good[1],diff(cumsum_good)),
           triangle = bad_inc*good_inc/2,
           rectangle = bad_inc*cumsum_good,
           area = rectangle-triangle) %$%
    sum(area)
  return(1-2*tot_area)
}
