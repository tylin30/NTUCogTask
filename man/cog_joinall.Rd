\name{cog_joinall}
\alias{cog_joinall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
cog_joinall
}
\description{
multi_join all task in one df
}
\usage{
cog_joinall(x)
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
all_ls <- list(SRTCRT_df, MA_df, MAO_df, DMS_df, DR_df, 
               SM_df, RMS_df, RMO_df, RML_df, SST_df, 
               As_df, CTT_df, Fg_df, HF_df, RS_df,
               Sp_df)

All_df <- cog_joinall(all_ls)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
