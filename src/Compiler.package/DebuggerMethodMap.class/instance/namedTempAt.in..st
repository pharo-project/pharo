accessing
namedTempAt: index in: aContext
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"
	^self
		privateTempAt: index
		in: aContext
		startpcsToBlockExtents: (self startpcsToBlockExtents: aContext method)