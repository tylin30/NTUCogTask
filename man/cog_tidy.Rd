\name{cog_tidy}
\alias{cog_tidy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cog tidy by task
}
\description{
this function is a pipeline from other data tidy function and saveRDS rawdata(combinded)&data unique by Subject in a list
(cog_rbindobs -> cog_datatype -> cog_mutate -> cog_unique -> save)
use this function and get cleaning data!
}
\usage{
cog_tidy(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
tylin
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cog_tidy(path, pattern, "SRTCRT")

cog_tidy("./all", ".*SRT_CRT.*.csv", "SRTCRT")

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
