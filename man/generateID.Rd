\name{generateID}
\alias{generateID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate ID codes for national courts
}
\description{
Script to generate ID codes for national courts in the National Courts data base compiled by Anna Wallerman et al.
}
\usage{
generateID(data,
           name = "courtID")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Data contains the national courts data base as an R data frame.
}
  \item{name}{
Name of ID variable to be inserted into first column of data frame.
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
Stein Arne Brekke
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

# # The national courts data base must be loaded
# # separately for this function to work.
# courts_data_with_ID <- generateID(courts_data_without_ID)

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
