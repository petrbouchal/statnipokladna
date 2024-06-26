% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/codelists.R
\name{sp_load_codelist}
\alias{sp_load_codelist}
\title{Load codelist into a tibble from XML file}
\usage{
sp_load_codelist(path, n = NULL)
}
\arguments{
\item{path}{Path to a file as returned by \code{sp_get_codelist_file()}}

\item{n}{Number of rows to return. Default (NULL) means all. Useful for quickly inspecting a codelist.}
}
\value{
a \link[tibble:tibble-package]{tibble}
}
\description{
This is normally called inside \code{sp_get_codelist()} but can be used separately if
finer-grained control of intermediate outputs is needed, e.g. in a \code{{targets}} workflow.
}
\examples{
\dontrun{
cf <- sp_get_codelist_file("druhuj")
sp_load_codelist(cf)
}
}
\seealso{
Other Detailed workflow: 
\code{\link{sp_get_codelist_file}()},
\code{\link{sp_get_codelist_url}()},
\code{\link{sp_get_dataset_url}()},
\code{\link{sp_get_table_file}()},
\code{\link{sp_load_table}()}
}
\concept{Detailed workflow}
