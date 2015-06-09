# Library
if(!require(DiagrammeR)) cat("Warning: missing DiagrammeR library. Some graphical capabilities are disabled. To install the required library run 'install.packages(\"DiagrammeR\")'.")

# Definitions
imp <- "->"
or <- "|"
and <- "&"
neg <- "~"
box <- "[]"
dia <- "<>"
#
t <- 0
places <- c()
tokens <- c()
transitions <- list()

agent <- function(label) {
	if(missing(label)) label <- paste("a", length(places)+1, sep="")
	places[length(places)+1] <<- label
	tokens[length(tokens)+1] <<- 0
	print(paste("Agent", label, "created"))
	return(length(places))
}

send <- function(agent1, agent2, freq=1) {
	if(missing(agent1) || missing(agent2)) stop("Missing agent to send data")
	transitions[[length(transitions)+1]] <<- c(1, agent1, agent2, freq)
	print(paste(agent1, "now able to send messages to", agent2, "with frequency", freq))
}

setDataCenters <- function(n=1, data=0) {
	print(paste("Creating", n, "data center(s)"))
	for(i in 1:n) {
		places[length(places)+1] <<- paste0("d", i)
		tokens[length(tokens)+1] <<- data[i]
	}
}

setMemory <- function(n=1) {
	print(paste("Creating", n, "shared memory(ies)"))
	for(i in 1:n) {
		places[length(places)+1] <<- paste0("m", i)
		tokens[length(tokens)+1] <<- 1
	}
}

collect <- function(agent, from=1, freq=1, shared) {
	if(missing(agent)) stop("Missing agent to collect data")
	rCenter <- pmatch(paste0("d",from), places)
	if(is.na(rCenter)){
		setDataCenters(n=from)
		rCenter <- length(places)
	}
	if(missing(shared)) transitions[[length(transitions)+1]] <<- c(1, rCenter, agent, freq)
	else {
		mem <- pmatch(paste0("m",shared), places)
		if(is.na(mem)) {
			setMemory(n=shared)
			mem <- length(places)
		}
		transitions[[length(transitions)+1]] <<- c(2, rCenter, mem, agent, freq)
		transitions[[length(transitions)+1]] <<- c(1, agent, mem, freq)
	}
	print(paste(agent, "now able to collect data with frequency", freq))
}

toPetriPDL <- function() {
	program <- ""
	for(i in 1:length(transitions))
		program <- paste0(program, switch(transitions[[i]][1],
			paste0(places[transitions[[i]][2]], "T", places[transitions[[i]][3]]),
			paste0(places[transitions[[i]][2]], ",", places[transitions[[i]][3]], "T", places[transitions[[i]][4]]),
			paste0(places[transitions[[i]][2]], "T", places[transitions[[i]][3]], ",", places[transitions[[i]][4]])
		), "*")
		return(substr(program,1,nchar(program)-1))
}

toDot <- function() {
	dotGraph <- "digraph G {"
	for(i in 1:length(places)) dotGraph <- paste0(dotGraph, "\n  place", i, "[label=", places[i], "];")
	for(i in 1:length(transitions)) {
		dotGraph <- paste0(dotGraph, "\n  transition", i, "[shape=box,style=filled,fillcolor=grey,label=\"\"];")
		dotGraph <- paste0(dotGraph, switch(transitions[[i]][1],
			paste0("\n  place", transitions[[i]][2], " -> transition", i, ";\n  transition", i," -> place", transitions[[i]][3], ";"),
			paste0("\n  place", transitions[[i]][2], " -> transition", i, ";\n  place", transitions[[i]][3], " -> transition", i, ";\n  transition", i," -> place", transitions[[i]][4], ";"),
			paste0("\n  place", transitions[[i]][2], " -> transition", i, ";\n  transition", i, " -> place", transitions[[i]][3], ";\n  transition", i," -> place", transitions[[i]][4], ";")
		))
	}
	return(paste0(dotGraph, "\n}"))
}

plotPN <- function() grViz(diagram=toDot())

setResource <- function(where, amount=1) {
	if(missing(where)) stop("Missing place to set")
	tokens[where] <<- amount
}

prToSend <- function(from, to) {
	if(missing(from) || missing(to)) stop("Missing agent to send data")
	a1 <- pmatch(from, places)
	if(is.na(a1)) stop("Agent ", from, " not found")
	a2 <- pmatch(to, places)
	if(is.na(a2)) stop("Agent ", to, " not found")
	if(tokens[a1] == 0) return(0)
	num <- 0
	den <- 0
	for(i in 1:length(transitions)) {
		if(switch(transitions[[i]][1], tokens[transitions[[i]][2]], tokens[transitions[[i]][2]] && tokens[transitions[[i]][3]], tokens[transitions[[i]][2]])) den <- den + transitions[[i]][length(transitions[[i]])]
		if(transitions[[i]][2] == a1 && transitions[[i]][3] == a2 && transitions[[i]][1] == 1) num <- transitions[[i]][4]
	}
	if(num == 0) {
		warning("Agent ", from, " unable to send data to agent ", to)
		return(0)
	}
	return(num/den)
}

fire <- function(s, p) {
	s1 <- table(strsplit(s, ",")[[1]])
	p1 <- strsplit(p, "T")[[1]]
	p2 <- strsplit(p1[1], ",")[[1]]
	if(!as.logical(max(names(s1) == p2[1])) || s1[[p2[1]]] == 0) return("")
	s1[[p2[1]]] <- s1[[p2[1]]] - 1
	if(length(p2) == 2) {
		if(!as.logical(max(names(s1) == p2[2])) || s1[[p2[2]]] == 0) return("")
		s1[[p2[2]]] <- s1[[p2[2]]] - 1
	}
	p3 <- strsplit(p1[2], ",")[[1]]
	if(as.logical(max(names(s1) == p3[1]))) s1[[p3[1]]] <- s1[[p3[1]]] + 1
	else s1[[p3[1]]] <- 1
	if(length(p3) == 2) {
		if(as.logical(max(names(s1) == p3[2]))) s1[[p3[2]]] <- s1[[p3[2]]] + 1
		else s1[[p3[2]]] <- 1
	}
	s2 <- ""
	for(i in 1:length(s1)) {
		if(s1[[i]] > 0) for(j in 1:s1[[i]]) s2 <- paste(s2, names(s1)[i], sep=",")
	}
	return(substr(s2, 2, nchar(s2)))
}

same.sequence <- function(s1, s2) as.logical(min(table(strsplit(s1,",")[[1]]) == table(strsplit(s2,",")[[1]])))

is.subprogram <- function(p1, p2) as.logical(max(strsplit(p2, "*", fixed=TRUE)[[1]] == p1))

is.basic <- function(p) !as.logical(length(strsplit(p, "*", fixed=TRUE)[[1]]))

apnf <- function(f) {
	if(!is.list(f)) return(f)
	if(f$op == box && is.list(f$r)) {
		if(f$r$op == neg) {
			if(f$r$r$op == imp) return(apnf(list(op=and, l=list(op=box,s=f$s,p=f$p,r=f$r$r$l), r=list(op=box,s=f$s,p=f$p,r=list(op=neg,r=f$r$r$r)))))
			if(f$r$r$op == or) return(apnf(list(op=and, l=list(op=box,s=f$s,p=f$p,list(op=neg,r=f$r$r$l)), r=list(op=box,s=f$s,p=f$p,r=list(op=neg,r=f$r$r$r)))))
			return(list(op=box, s=f$s, p=f$p, r=apnf(f$r)))
		}
		if(f$r$op == imp) return(apnf(list(op=imp, l=list(op=box,s=f$s,p=f$p,r=f$r$l), r==list(op=box,s=f$s,p=f$p,r=f$r$r))))
		if(f$r$op == and) return(apnf(list(op=and, l=list(op=box,s=f$s,p=f$p,r=f$r$l), r==list(op=box,s=f$s,p=f$p,r=f$r$r))))
		if(f$r$op == box || f$r$op == dia) {
			if(is.basic(f$p) && is.subprogram(f$p, f$r$p) && same.sequence(f$r$s, fire(s=f$s, p=f$p))) return(apnf(list(op=dia, s=f$s, p=f$r$p, r=f$r$r)))
			return(apnf(list(op=box, s=f$s, p=f$p, r=apnf(f$r))))
		}
	}
	if(f$op == dia && is.list(f$r)) {
		if(f$r$op == neg) {
			if(f$r$r$op == and) return(apnf(list(op=or, l=list(op=dia,s=f$s,p=f$p,r=list(op=neg,r=f$r$r$l)), r=list(op=dia,s=f$s,p=f$p,r=list(op=neg,r=f$r$r$r)))))
			return(list(op=dia, s=f$s, p=f$p, r=apnf(f$r)))
		}
		if(f$r$op == imp) return(apnf(list(op=or, l=list(op=dia,s=f$s,p=f$p,r=list(op=neg,r=f$r$l)), r==list(op=dia,s=f$s,p=f$p,r=f$r$r))))
		if(f$r$op == or) return(apnf(list(op=or, l=list(op=box,s=f$s,p=f$p,r=f$r$l), r=list(op=box,s=f$s,p=f$p,r=f$r$r))))
		if(f$r$op == box || f$r$op == dia) {
			if(is.basic(f$p) && is.subprogram(f$p, f$r$p) && same.sequence(f$r$s, fire(s=f$s, p=f$p))) return(apnf(list(op=dia, s=f$s, p=f$r$p, r=f$r$r)))
			return(apnf(list(op=dia, s=f$s, p=f$p, r=apnf(f$r))))
		}
	}
	if(f$op == neg && is.list(f$r)) {
		if(f$r$op == box) return(apnf(list(op=dia, s=f$r$s, p=f$r$p, r=list(op=neg, r=f$r$r))))
		if(f$r$op == dia) return(apnf(list(op=box, s=f$r$s, p=f$r$p, r=list(op=neg, r=f$r$r))))
		if(f$r$op == imp) return(list(op=and, l=apnf(f$r$l), r=apnf(list(op=neg, r=f$r$r))))
		if(f$r$op == and) return(list(op=or, l=apnf(list(op=neg, r=f$r$l)), r=apnf(list(op=neg, r=f$r$r))))
		if(f$r$op == or) return(list(op=and, l=apnf(list(op=neg, r=f$r$l)), r=apnf(list(op=neg, r=f$r$r))))
	}
	if(f$op == imp) return(list(op=and, l=apnf(f$l), r=list(op=neg, r=apnf(f$r))))
	if(f$op == and) return(list(op=and, l=apnf(f$l), r=apnf(f$r)))
	if(f$op == or) return(list(op=or, l=apnf(f$l), r=apnf(f$r)))
	return(f)
}

dsnf <- function(f) list(I="t0", U=rewriteDSNF(list(op=imp, l=t0, r=apnf(f))))

#OnGoing
rewriteDSNF <- function(f, U) {
	if(missing(U)) U <- list()
	else f <- U[[1]]
	if(is.list(f$r$r)) {
		if(f$r$op == and) {
			f1 <- rewriteDSFN(list(op=imp, l=f$l, r=f$r$l))
			f2 <- rewriteDSFN(list(op=imp, l=f$l, r=f$r$r))
		}
		if(f$r$op == or && is.list(f$r$l) && ((f$r$l$op != dia && f$r$l$op != box) || is.list(f$r$l$r) )) {
			t <<- t + 1
			f1 <- rewriteDSFN(list(op=imp, l=f$l, r=list(op=or, l=f$r$r, r=paste0("t",t))))
			f2 <- rewriteDSFN(list(op=imp, l=paste0("t",t), r=f$r$l))
		}
		else if(f$r$op == or && ((f$r$r$op != dia && f$r$r$op != box) || is.list(f$r$r$r))) {
			t <<- t + 1
			f1 <- rewriteDSFN(list(op=imp, l=f$l, r=list(op=or, l=f$r$l, r=paste0("t",t))))
			f2 <- rewriteDSFN(list(op=imp, l=paste0("t",t), r=f$r$r))
		}
		if((f$r$op == dia || f$r$op == box) && is.list(f$r$r)) {
			t <<- t + 1
			f1 <- rewriteDSFN(list(op=imp, l=f$l, r=list(op=f$r$op, s=f$r$s, r=paste0("t",t))))
			f2 <- rewriteDSFN(list(op=imp, l=paste0("t",t), r=f$r$r))
		}
		U[length(U)+1:length(U)+length(f1)] <- f1
		U[length(U)+1:length(U)+length(f2)] <- f2
		return(U)
	}
	return(list(f))
}