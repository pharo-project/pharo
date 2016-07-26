instance creation
selector: aSymbol argument: anObject 
	"Answer an instance of me whose selector is aSymbol and single 
	argument is anObject."

	^self new setSelector: aSymbol arguments: { anObject }