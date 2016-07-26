instance creation
selector: aSymbol
	"Answer an instance of me with unary selector, aSymbol."

	^self new setSelector: aSymbol arguments: (Array new: 0)