\name{brew}
\alias{brew}
\title{Report Brewing For Text and R Output}
\description{
  \code{brew} provides a templating system for text reporting. The syntax is similar to PHP,
  Java Server Pages, Ruby's erb module, and Python's psp module.
}
\usage{
brew(file=stdin(),output=stdout(),text=NULL,envir=parent.frame(),
     run=TRUE,parseCode=TRUE,tplParser=NULL,chdir=FALSE,
     delim.code=c('<\%','\%>'),delim.comment=c('<\%#','\%>'),
     delim.catcode=c('<\%=','\%>'),delim.template=c('<\%\%','\%\%>'))
}
\arguments{
  \item{file}{A connection, or a character string naming the file
    to read from. stdin() is the default.}
  \item{output}{A connection, or a character string naming the file
    to print to. stdout() is the default.}
  \item{text}{ A character string treated as if it contained lines of a file
    to read from. Only one of \code{file} or \code{text} is used as input. 
    Default is \code{NULL}.}
  \item{envir}{the \code{\link{environment}} in which the input is to
    be evaluated.  Default is the caller's environment, useful for 
    nested \code{brew} calls.}
  \item{run}{Logical to determine if \code{brew} should evaluate the input (\code{run=TRUE}) or 
    just parse it (\code{run=FALSE}). Useful for debugging.}
  \item{parseCode}{Logical. only relevant when run=FALSE. When TRUE the brewed code is parsed and then silently returned. When FALSE, the brewed code is returned as a list. See the Value section for details.}
  \item{tplParser}{a function to parse the text between '<\%\%' and '\%\%>' and return the result as
    a character vector. The template text is passed to the function as a variable
    length character vector in the first argument position.}
  \item{chdir}{logical; if \code{TRUE} and \code{file} is a pathname, the R working
          directory is temporarily changed to the directory containing
          \code{file} for evaluating. \code{brew} will also honor the global option \code{brew.chdir}.}
  \item{delim.code}{Delimiters for executing code}
  \item{delim.comment}{Delimiters for comments}
  \item{delim.catcode}{Delimiters for executing code and printing the results}
  \item{delim.template}{Delimiters for nested templating}
}
\details{
	brew syntax is quite simple and there are very few delimiters to learn:

	\itemize{
	\item{1.}{All text that falls outside of the delimiters is printed as-is.}
	\item{2.}{R expressions between the '<\%' and '\%>' delimiters are executed in-place.}
    \item{3.}{The value of the R expression between the '<\%=' and '\%>' delimiters is printed.}
	\item{4.}{All text between the '<\%\#' and '\%>' delimiters is thrown away. Use it as a comment.}
	\item{5.}{If you place a '-' just before the '\%>' delimiter, and it's placed at the end of a line, then the newline is omitted from the output.}
	}
    
	The following template contains syntax to exercise all \code{brew} functionality:

\preformatted{
---------------
You won't see this R output, but it will run. <\% foo <- 'bar' \%>
Now foo is <\%=foo\%> and today is <\%=format(Sys.time(),'\%B \%d, \%Y')\%>.
<\%# Comment -- ignored -- useful in testing. 
    Also notice the dash-percent-gt.
    It chops off the trailing newline. 
    You can add it to any percent-gt. -\%>
How about generating a template from a template?
<\%\% foo <- 'fee fi fo fum' \%\%>
foo is still <\%=foo\%>.
---------------
}

	The output is:

\preformatted{
--------------
You won't see this R output, but it will run.
Now foo is bar and today is April 20, 2007.
How about generating a template from a template?
<\% foo <- 'fee fi fo fum' \%>
foo is still bar.
--------------
}

	Also, for power users, there are a couple more things:

	\itemize{
	\item{6.}{Text between the '<\%\%' and '\%\%>' delimiters is treated as a brew template and
      is printed as-is, but the delimiters are changed to '<\%' and '\%>'. This happens when tplParser=NULL. But if tplParser is a valid function, then the text is passed to tplParser which should return a character vector to replace the text.}
	\item{7.}{You can customize the delimiters that brew uses by setting the delim arguments: \code{delim.code}, \code{delim.comment}, \code{delim.catcode}, and \code{delim.template.}}
	}

	NOTE: brew calls can be nested and rely on placing a function named '.brew.cat' in the environment in which it is passed. Each time brew is called, a check for the existence of this function is made. If it exists, then it is replaced with a new copy that is lexically scoped to the current brew frame. Once the brew call is done, the function is replaced with the previous function. The function is finally removed from the environment once all brew calls return.

}
\value{
	When \code{run=TRUE}, the value of the last expression after brewing the input or an object of class 'try-error' containing the error message if brewing failed.

    When \code{run=FALSE} and \code{parseCode=TRUE}, a function whose environment contains the text vector and the code vector of the parsed expressions after brewing the input. It takes brew's output and envir arguments.

    When \code{run=FALSE} and \code{parseCode=FALSE}, a list containing the text vector and the unparsed code vector.
}
\author{ Jeffrey Horner <jeff.horner@vanderbilt.edu> }
\seealso{ \code{\link{Sweave}} for the original report generator. }
\examples{

## A port of the Sweave test file.
brew(system.file("brew-test-1.brew",package="brew"),"brew-test-1.tex",envir=new.env())

## Everything you wanted to know about your R session.
brew(system.file("brew-test-2.brew",package="brew"),"brew-test-2.html",envir=new.env())
browseURL(paste('file://',file.path(getwd(),'brew-test-2.html'),sep=''))

## Don't sully up environment, so use envir=new.env(). Nested brew calls will still work.
brew(system.file("example1.brew",package="brew"),envir=new.env())

## Various ways to print R output 
library(datasets)
brew(system.file("catprint.brew",package="brew"),envir=new.env())
rm(iris)

## The example from the Details section
brew(system.file("featurefull.brew",package="brew"),envir=new.env())

## Using the tplParser argument
tParse <- function(text) paste('Got this: <',text,'>\n',sep='',collapse='')
brew(system.file("featurefull.brew",package="brew"),envir=new.env(),tplParser=tParse)
rm(tParse)
}
\keyword{utilities}
