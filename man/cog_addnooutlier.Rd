\name{cog_addnooutlier}
\alias{cog_addnooutlier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cog_addnooutlier
}
\description{
add new col no outlier RT.

outlier will label to NaN
}
\usage{
cog_addnooutlier(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
df: df
Task: Task name
range: range to define outlier (default = 2.5)
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
sd <- cog_addnooutlier(sst_df, "SST")

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
