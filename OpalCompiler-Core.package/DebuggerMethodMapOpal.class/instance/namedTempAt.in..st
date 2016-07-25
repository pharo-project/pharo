public
namedTempAt: index in: aContext
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"

	| name |	
	name := (self tempNamesForContext: aContext) at: index.
	^self tempNamed: name in: aContext.