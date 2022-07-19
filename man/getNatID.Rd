\name{getNatID}
\alias{getNatID}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get ID code from name(s) of national court(s)
}
\description{
Script to identify ID codes based on the name and contry of national
courts in the national courts data base.
}
\usage{
getNatID(court,
	 data = NA,
	 country = NA,
	 flatten = TRUE)
}

\arguments{
  \item{court}{
Name of the court(s).
}
  \item{data}{
National courts data frame.
}
  \item{country}{
Country in which the court is located.
}
  \item{flatten}{
Whether or not to always present output as a flat list. Applies when several
courts are identified in one text string of `court`.
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
# ID <- getNatID("Bundesverfassungsgericht",
#		 data = data,
#		 country = "Germany")

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

