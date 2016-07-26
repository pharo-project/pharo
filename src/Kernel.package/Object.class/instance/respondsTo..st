class membership
respondsTo: aSymbol 
	"Answer whether the method dictionary of the receiver's class contains 
	aSymbol as a message selector."

	^self class canUnderstand: aSymbol