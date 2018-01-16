\name{step}
\alias{step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
step
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
UMBEL Models with Backward ELimination.
performs automatic backward elimination of all effects of linear mixed effect model
and returns a list of the backward eliminated models.
}
\usage{
step(model, i = 0, beeping = F, ps = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{model} here~~
}
%  \item{i}{
%%     ~~Describe \code{i} here~~
}
%  \item{beeping}{
%%     ~~Describe \code{beeping} here~~
}
%  \item{ps}{
%%     ~~Describe \code{ps} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (model, i = 0, beeping = F, ps = list())
{
    require(magrittr)
    require(lme4)
    require(beepr)
    if (beeping) {
        beep()
    }
    message("###########################################")
    print(i)
    model_summary = summary(model)
    rand_factors = (model_summary$ngrps \%>\% as.data.frame() \%>\%
        rownames() \%>\% as.list())
    variances = model_summary$varcor
    stds = list()
    for (rand_factor in rand_factors) {
        rand_factor_std = sprintf("std_\%s", rand_factor)
        get_rand_factor_std = sprintf("attr(variances$\%s,\"stddev\")",
            rand_factor)
        assign(rand_factor_std, as.data.frame(eval(parse(text = get_rand_factor_std))))
        stds = append(stds, rand_factor_std)
    }
    min_std = 10000
    min_list = list()
    for (rand_factor in rand_factors) {
        if (nrow(get(sprintf("std_\%s", rand_factor))) == 1)
            next
        tmp_min = min(get(sprintf("std_\%s", rand_factor))[-1,
            ])
        if (tmp_min <= min_std) {
            min_std = tmp_min
            list_of_vars = get(sprintf("std_\%s", rand_factor))[,
                1]
            min_index = which(list_of_vars == min_std)[1]
            min_name = rownames(get(sprintf("std_\%s", rand_factor)))[min_index]
            min_list = c(rand_factor, min_name, min_std)
        }
    }
    print(min_list)
    rownames(get(sprintf("std_\%s", rand_factor)))
    rand_intercepts = list()
    for (rand_factor in rand_factors) {
        if (rand_factor == min_list[1]) {
            x = rownames(get(sprintf("std_\%s", rand_factor)))[-1]
            y = x[x != min_list[2]]
            if (length(y) == 0) {
                rand_intercept = sprintf("(1 |\%s)", rand_factor)
            }
            else {
                z = paste(y, collapse = " + ")
                rand_intercept = sprintf("(1 + \%s|\%s)", z, rand_factor)
            }
            rand_intercepts = append(rand_intercepts, rand_intercept)
        }
        else {
            x = rownames(get(sprintf("std_\%s", rand_factor)))[-1]
            y = x
            if (length(y) == 0) {
                rand_intercept = sprintf("(1 |\%s)", rand_factor)
            }
            else {
                z = paste(y, collapse = " + ")
                rand_intercept = sprintf("(1 + \%s|\%s)", z, rand_factor)
            }
            rand_intercepts = append(rand_intercepts, rand_intercept)
        }
    }
    rs = paste(rand_intercepts, collapse = " + ")
    formula = as.character(model_summary$call)
    lme_function = formula[1]
    lme_formula = formula[2]
    lme_data = formula[3]
    dependent_independent = strsplit(lme_formula, "\\(")[[1]][1]
    new_line = paste(dependent_independent, rs)
    message(paste("The old formula is: ", lme_formula))
    print(variances)
    message(paste("The old formula is: ", lme_formula))
    message(sprintf("removed \%s in \%s", min_list[2], min_list[1]))
    message(paste("The new formula is: ", new_line))
    i = i + 1
    print(min_list)
    if (min_list[2][[1]] \%>\% is.null) {
        if (beeping) {
            beep(5)
        }
        return(ps)
    }
    old_model_name = deparse(substitute(model))
    new_model_name = paste(old_model_name, as.character(i), sep = "_")
    str_formula = sprintf("\%s = lmer(\%s, data=\%s)", new_model_name,
        new_line, lme_data)
    print(str_formula)
    eval(parse(text = str_formula))
    appender = sprintf("ps = append(ps, \%s)", new_model_name)
    eval(parse(text = appender))
    print(appender)
    print(ps)
    recall = sprintf("step(\%s, i, beeping, ps=ps)",
        new_model_name)
    eval(parse(text = recall))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
