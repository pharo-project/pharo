debugger access
namedTempAt: index put: aValue
	"Set the value of the temp at index in the receiver's sequence of tempNames.
	 (Note that if the value is a copied value it is also set out along the lexical chain,
	  but alas not in along the lexical chain.)."
	^self debuggerMap namedTempAt: index put: aValue in: self