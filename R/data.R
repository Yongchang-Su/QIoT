#' Cadmium level in blood after smoking
#' 
#' Data from the 2005-2006 National Health and Nutrition Examination Survey to study how smoking influence the cadmium level in blood.
#' @docType data
#' @keywords datasets
#' @name cadmium
#' @usage data(cadmium)
#' @format A data frame with 1536 rows and 34 variables:
#' \describe{
#'   \item{Row}{a numeric vector}
#'   \item{SEQN}{NHANES id number}
#'   \item{female}{1 if female, 0 if male}
#'   \item{age}{age in years, >=20}
#'   \item{black}{1 if black, 0 otherwise}
#'   \item{hispanic}{1 if hispanic, 0 otherwise}
#'   \item{education}{Education}
#'   \item{povertyr}{Ratio of family income to the poverty level, capped at 5x}
#'   \item{creactiveprotein}{creactive protein}
#'   \item{homocysteine}{homocysteine}
#'   \item{cotinine}{cotinine in blood}
#'   \item{cadmium}{cadmium in blood}
#'   \item{lead}{lead in blood}
#'   \item{bmi}{Body mass index}
#'   \item{cigs100life}{1 if smoked more than 100 cigarettes in lifetime, 0 otherwise}
#'   \item{smokenow}{1 if smokes now, 0 otherwise}
#'   \item{cigsdays30}{Days smoked in last 30 days: 0 if never smoker, 30 if daily smoker}
#'   \item{cigsperday30}{Cigarettes smoked per day in last 30 days}
#'   \item{tobacco5days}{1 = used tobacco in the last 30 days, 0 otherwise}
#'   \item{dailysmoker}{1 = daily smoker, 0 = never smoker}
#'   \item{neversmoker}{1 = never smoker, 0 = daily smoker}
#'   \item{z}{1 if daily smoker, 0 if never smoker}
#'   \item{propens}{Estimated propensity score. The score was formed by logit regression of z on female, age, education, black, hispanic, povertyr, and bmi.}
#'   \item{pstrat}{Propensity score strata: (0,0.0733] (0.0733,0.131] (0.131,0.204] (0.204,0.33] (0.33,1]}
#'   \item{age3}{Age in 3 categories}
#'   \item{ed3}{Education in 3 categories}
#'   \item{bmi3}{BMI in 3 categories}
#'   \item{pov2}{Income above 2 times poverty, TRUE or FALSE}
#'   \item{stf}{A factor defining strata using female, age3, ed3, bmi3 pov2.}
#'   \item{st}{A numeric version of stf}
#'   \item{stfp}{A factor defining strata using stf and pstrat}
#'   \item{stp}{A numeric version of stp}
#'   \item{p}{Propensity score obtained by performing optimal match.}
#'   \item{mset}{Group id after matching}
#' }
#' @source \href{https://cran.r-project.org/web/packages/bigmatch/index.html}
"cadmium"