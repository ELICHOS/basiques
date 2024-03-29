% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_bool.R
\name{create.bool}
\alias{create.bool}
\title{La fonction "create.bool"}
\usage{
create.bool(dataframe = dataclust, factor.vars = "Q8.Ress1")
}
\arguments{
\item{dataframe}{un data.frame}

\item{factor.vars}{le nom (char) ou l'indexe de la variable facteur d'intérêt}
}
\value{
un data.frame
}
\description{
Cré un data.frame avec autant de variables booléennes que de modalités présentes dans la variable factor.vars
}
\examples{
create.bool(dataframe=dataclust, factor.vars="Q8.Ress1")
}
\references{
no
}
\author{
Elie
}
