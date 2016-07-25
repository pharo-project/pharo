rounding
cornerStyle: aSymbol
	"Set the receiver's corner style.  But, in this case, do *not*"

	(extension isNil or: [self cornerStyle == aSymbol]) ifTrue: [^self].
	extension cornerStyle: nil.
	self changed