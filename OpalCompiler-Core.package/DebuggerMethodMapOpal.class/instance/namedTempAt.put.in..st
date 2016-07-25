public
namedTempAt: index put: aValue in: aContext
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	
	| name |	
	name := (self tempNamesForContext: aContext) at: index.
	^self tempNamed: name in: aContext put: aValue.