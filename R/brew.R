#
#  brew - text templating for R (www.r-project.org)
#
#  Copyright (C) 2007 Jeffrey Horner
#
#  brew is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

BRTEXT <- 1
BRCODE <- 2
BRCOMMENT <- 3
BRCATCODE <- 4
BRTEMPLATE <- 5

.bufLen <- 0
.cache <- NULL

# Experimental function for setting the size of internal buffers.
# There's anecdotal evidence that suggests larger buffer sizes
# means faster parsing. 
setBufLen <- function(len=0){
	.bufLen <<- len
	invisible(NULL)
}

brewCache     <- function(envir=NULL) {
	if (missing(envir)) return(.cache)
	.cache <<- envir
	invisible(NULL)
}
brewCacheOn  <- function() brewCache(new.env(hash=TRUE,parent=globalenv()))
brewCacheOff <- function() brewCache(NULL)

# Find a delimiter
brewFindDelim <- function(line, delim, opening = TRUE) {
	# Find the longest delimiter that matches the earliest
	matches <- list()
	match.pos <- structure(nchar(line) + 1L, match.length = -1L)
	match.which <- 0

	# Start at 2 because 1 (the text delimiter) is empty
	for (i in 2:length(delim)) {
		pattern <- if (opening) delim[[i]][1] else delim[[i]][2]
		pos <- regexpr(pattern,line,fixed=TRUE)
		if (pos > 0 && pos <= match.pos && attr(pos, 'match.length') >= attr(match.pos, 'match.length')) {
			if (pos < match.pos || attr(pos, 'match.length') > attr(match.pos, 'match.length')) {
				matches <- list()
				match.pos <- pos
				match.which <- i
			}
			matches[[length(matches) + 1]] <- list(which = i, pos = pos)
		}
	}
	matches
}

`.brew.cached` <- function(output=stdout(),envir=parent.frame()){
	# Only sink if caller passed an argument
	sunk <- FALSE
	if (!missing(output)) {
		sunk <- TRUE
		sink(output)
	}

	text <- get('text')
	brew.cat <- function(from,to) cat(text[from:to],sep='',collapse='')
	.prev.brew.cat <- NULL
	if (exists('.brew.cat',envir=envir)){
		.prev.brew.cat <- get('.brew.cat',pos=envir)
	}
	assign('.brew.cat',brew.cat, envir=envir)

	code <- get('code')
	ret <- try(eval(code,envir=envir))

	# sink() will warn if trying to end the real stdout diversion
	if (sunk && unclass(output) != 1) sink()

	if(!is.null(.prev.brew.cat)){
		assign('.brew.cat',.prev.brew.cat,envir=envir)
	} else {
		rm('.brew.cat',envir=envir)
	}

	invisible(ret)
}

`brew` <-
function(file=stdin(),output=stdout(),text=NULL,envir=parent.frame(),run=TRUE,parseCode=TRUE,tplParser=NULL,chdir=FALSE,delim.code=c('<%','%>'),delim.comment=c('<%#','%>'),delim.catcode=c('<%=','%>'),delim.template=c('<%%','%%>')){

	file.mtime <- canCache <- isFile <- closeIcon <- FALSE
	filekey <- file # we modify file when chdir=TRUE, so keep same cache key

	# Error check input
	if (is.character(file) && file.exists(file)){
		isFile <- closeIcon <- TRUE
		if (chdir || isTRUE(getOption('brew.chdir'))){
			if((path <- dirname(file)) != ".") {
				owd <- getwd()
				if(is.null(owd))
					warning("cannot 'chdir' as current directory is unknown")
				else on.exit(setwd(owd), add=TRUE)
				setwd(path)
				file <- basename(file)
			}
		}

	} else if (is.character(file) && grepl("^(ftp|http|file)://", file)) {
		isFile <- closeIcon <- TRUE
                if(chdir || isTRUE(getOption('brew.chdir')))
			warning("'chdir = TRUE' makes no sense for a URL")
	} else if (is.character(text) && nchar(text[1]) > 0){
		closeIcon <- TRUE
		icon <- textConnection(text[1])
	} else if (inherits(file,'connection') && summary(file)$"can read" == 'yes') {
		icon <- file
	} else {
		stop('No valid input')
		return(invisible(NULL))
	}

	# Error check output
	if (inherits(output,'connection')){
		if (summary(output)$"can write" != 'yes')
			stop('output connection is not writeable')
	} else if ( !is.character(output) )
		stop('No valid output')

	# Error check env
	if (!is.environment(envir)){
		warning('envir is not a valid environment')
		return(invisible(NULL))
	}

	# Error check delimiters
	if (!is.character(delim.code) || length(delim.code) != 2) {
		stop("delim.code must be a character vector of length 2")
	}
	if (!is.character(delim.comment) || length(delim.comment) != 2) {
		stop("delim.comment must be a character vector of length 2")
	}
	if (!is.character(delim.catcode) || length(delim.catcode) != 2) {
		stop("delim.catcode must be a character vector of length 2")
	}
	if (!is.character(delim.template) || length(delim.template) != 2) {
		stop("delim.template must be a character vector of length 2")
	}

	# Can we use the cache
	if (!is.null(.cache) && isFile && run && is.null(tplParser)){
		canCache <- TRUE
		if (exists(filekey,.cache)){
			file.cache <- get(filekey,.cache)
			file.mtime <- file.info(file)$mtime
			if (file.cache$mtime >= file.mtime){
				brew.cached <- .brew.cached
				environment(brew.cached) <- file.cache$env
				if (!missing(output)) {
					return(brew.cached(output,envir))
				} else {
					return(brew.cached(envir=envir))
				}
			}
		}
	}

	# Not using cache, open input file if needed
	if (isFile) icon <- file(file,open="rt")

	delim <- list()
	delim[[BRCODE]] <- delim.code
	delim[[BRCOMMENT]] <- delim.comment
	delim[[BRCATCODE]] <- delim.catcode
	delim[[BRTEMPLATE]] <- delim.template
	state <- BRTEXT
	text <- code <- tpl <- character(.bufLen)
	textLen <- codeLen <- as.integer(0)
	textStart <- as.integer(1)
	line <- ''
	
	while(TRUE){
		if (!nchar(line)){
			line <- readLines(icon,1)
			if (length(line) != 1) break
			line <- paste(line,"\n",sep='')
		}
		if (state == BRTEXT){
			# Find opening delimiter
			d <- brewFindDelim(line, delim, opening = TRUE)

			if (length(d) > 0) {
				# Beginning markup found
				d <- d[[1]]
				state <- d$which
				if (d$pos > 1) {
					text[textLen+1] <- substr(line, 1, d$pos - 1)
					textLen <- textLen + 1
				}
				line <- substr(line, d$pos + attr(d$pos, 'match.length'), nchar(line))

				if (state == BRTEMPLATE){
					if (is.null(tplParser)){
						text[textLen+1] <- delim[[BRCODE]][1]
						textLen <- textLen + 1
					}
					next
				}
				if (textStart <= textLen) {
					code[codeLen+1] <- paste('.brew.cat(',textStart,',',textLen,')',sep='')
					codeLen <- codeLen + 1
					textStart <- textLen + 1
				}
			} else {
				text[textLen+1] <- line
				textLen <- textLen + 1
				line <- ''
			}
		} else {
			# Find closing delimiter
			d <- brewFindDelim(line, delim, opening = FALSE)
			if (length(d) == 0) {
				# Didn't find a closing tag
				# Complain if we see another opening tag
				d2 <- brewFindDelim(line, delim, opening = TRUE)
				if (length(d2) > 0) {
					stop("Oops! Someone forgot to close a tag. We saw: ",delim[[state]][1],' and we need ',delim[[state]][2])
				}

				if (state == BRTEMPLATE && !is.null(tplParser))
					tpl[length(tpl)+1] <- line
				else {
					text[textLen+1] <- line
					textLen <- textLen + 1
				}
				line <- ''
			} else {
				# brewFindDelim will return more than one match if there is more than
				# one closing tag that are the same. We'll try to determine the
				# correct closing tag to use before complaining.
				tag.index <- 0
				for (i in 1:length(d)) {
					if (d[[i]]$which == state) {
						tag.index <- i
						break
					}
				}
				if (tag.index == 0) {
					# Wrong closing tag
					stop("Oops! Someone forgot to close a tag. We saw: ",delim[[state]][1],' and we need ',delim[[state]][2])
				}
				d <- d[[tag.index]]
				part1 <- substr(line, 0, d$pos - 1)
				part2 <- substr(line, d$pos + attr(d$pos, 'match.length'), nchar(line))
				if (state == BRTEMPLATE){
					if (!is.null(tplParser)){
						tpl[length(tpl)+1] <- part1
						# call template parser
						tplBufList <- tplParser(tpl)
						if (length(tplBufList)){
							textBegin <- textLen + 1;
							textEnd <- textBegin + length(tplBufList) - 1
							textLen <- textEnd
							text[textBegin:textEnd] <- tplBufList
						}
						tpl <- character()
					} else {
						text[textLen+1] <- paste(part1,delim[[BRCODE]][2],sep='')
						textLen <- textLen + 1
					}
					line <- part2
					state <- BRTEXT
					next
				} else {
					n <- nchar(part1)
					# test  for '-' immediately preceding %> will strip trailing newline from line
					if (n > 0) {
						if (substr(part1,n,n) == '-') {
							part2 <- substr(part2,1,nchar(part2)-1)
							part1 <- substr(part1,1,n-1)
						}
						text[textLen+1] <- part1
						textLen <- textLen + 1
					}
					line <- part2

					# We've found the end of a brew section, but we only care if the
					# section is a BRCODE or BRCATCODE. We just implicitly drop BRCOMMENT sections
					if (state == BRCODE){
						code[codeLen+1] <- paste(text[textStart:textLen],collapse='')
						codeLen <- codeLen + 1
					} else if (state == BRCATCODE){
						code[codeLen+1] <- paste('cat(',paste(text[textStart:textLen],collapse=''),')',sep='')
						codeLen <- codeLen + 1
					}
					textStart <- textLen + 1
					state <- BRTEXT
				}
			}
		}
	}
	if (state == BRTEXT){
		if (textStart <= textLen) {
			code[codeLen+1] <- paste('.brew.cat(',textStart,',',textLen,')',sep='')
			codeLen <- codeLen + 1
			textStart <- textLen + 1
		}
	} else {
		stop("Oops! Someone forgot to close a tag. We saw: ",delim[[state]][1],' and we need ',delim[[state]][2])
	}

	if (closeIcon) close(icon)

	if (run){

		brew.env <- new.env(parent=globalenv())
		assign('text',text,brew.env)
		assign('code',parse(text=code,srcfile=NULL),brew.env)
		brew.cached <- .brew.cached
		environment(brew.cached) <- brew.env

		if (canCache){
			if (file.mtime == FALSE) file.mtime <- file.info(file)$mtime
			assign(filekey,list(mtime=file.mtime,env=brew.env),.cache)
		}

		if (!missing(output)) {
			return(brew.cached(output,envir))
		} else {
			return(brew.cached(envir=envir))
		}
	} else if (parseCode){
		brew.env <- new.env(parent=globalenv())
		assign('text',text,brew.env)
		assign('code',parse(text=code,srcfile=NULL),brew.env)
		brew.cached <- .brew.cached
		environment(brew.cached) <- brew.env
		invisible(brew.cached)
	} else {
		invisible(list(text=text,code=code))
	}
}

.onAttach <- function(library, pkg)
{
	unlockBinding('.bufLen',asNamespace('brew'))
	unlockBinding('.cache',asNamespace('brew'))
}
