% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardise_plots.R
\name{standardise_plots}
\alias{standardise_plots}
\title{Standardise all plot identifying columns}
\usage{
standardise_plots(dataframe, output_dir, file_path)
}
\arguments{
\item{dataframe}{The dataframe that is to have its plot descriptor data standardised}

\item{output_dir}{The directory to which a warning file may be written if mixed treatment columns
are encountered}

\item{file_path}{The file_path that will be used to describe the origin of the data in a potential
warning file if mixed treatment columns are encountered}
}
\description{
A function which accepts a dataframe and returns it with its plot descriptor data
standardised. Input dataframe should include a column named "site" detailing the site at Alex
(eg Cassiope) and a column named "plot" containing the plot_number. If no "plot" column is
present then the data is assumed to be site, rather than plot level, data and is labeled
accordingly in the created plot_id as well as being given the plot number 0.
Standardise_plots combines the functions:
standardise_plot_col_names - to ensure all column names are consistent
standardise_sites - To ensure that the site names are consistent
standardise_plot_number - To ensure any additional data stored within the plot column is moved to
the appropriate column
standardise_treatment - To ensure treatment names are consistent and treatment types (otc vs snow)
are not mixed
get_plot_ID - To look up the correct unique plot IDs from the plot_names dataframe
The exact workings of each of these functions is described in their own help files. The output_dir
and file_path are also needed to produce possible warning files if the separating of snow and otc
treatment data is uncertain.
The final result should follow the same format as the plot_names file included within the tundra
package and accessible running data(plot_names).
}
