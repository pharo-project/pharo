accessing
tempNamesForContext: aContext
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^(self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: (self startpcsToBlockExtents: aContext method)) collect:
			[:pair| pair first]