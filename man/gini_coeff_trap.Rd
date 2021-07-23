\name{gini_coeff_trap}
\alias{gini_coeff_trap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Claculate Gini Coefficient using Trapezium Rule
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gini_coeff_trap(score_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{score_data}{
%%     ~~Describe \code{score_data} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (score_data)
{
    names(score_data) <- c("score", "bad")
    tot_area <- score_data \%>\% mutate(good = 1 - bad) \%>\% group_by(score) \%>\%
        summarise(bad = sum(bad), good = sum(good)) \%>\% mutate(cumsum_bad = cumsum(bad)/sum(bad),
        cumsum_good = cumsum(good)/sum(good), bad_inc = c(cumsum_bad[1],
            diff(cumsum_bad)), good_inc = c(cumsum_good[1], diff(cumsum_good)),
        triangle = bad_inc * good_inc/2, rectangle = bad_inc *
            cumsum_good, area = rectangle - triangle) \%$\% sum(area)
    return(1 - 2 * tot_area)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
