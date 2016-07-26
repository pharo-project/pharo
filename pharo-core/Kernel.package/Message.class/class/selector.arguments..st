instance creation
selector: aSymbol arguments: anArray 
	"Answer an instance of me with selector, aSymbol, and arguments, 
	anArray."

	^self new setSelector: aSymbol arguments: anArray