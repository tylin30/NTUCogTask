\name{cog_subrename}
\alias{cog_subrename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rename ncku001a, ncku001b, ncku001c to ncku001
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cog_subrename(x)
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
srtcrt_df <- cog_tidy("datafolderpath", "SRTCRT") \%>\% cog_subrename()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
