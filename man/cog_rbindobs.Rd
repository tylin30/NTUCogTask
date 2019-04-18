\name{cog_rbindobs}
\alias{cog_rbindobs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rbind one task all observers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cog_rbindobs(path, Task)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{path}{
  file path
%%     ~~Describe \code{path} here~~
}
  \item{Regex_pattern}{
  the pattern you would like to match
%%     ~~Describe \code{Regex_pattern} here~~
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
df <- cog_rbindobs("./all", "SRTCRT")
you can also type RegrexPatern in Task par
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
