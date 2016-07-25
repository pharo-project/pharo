enumerating
collect: aBlock
	"Answer a new matrix with transformed elements; transformations should be independent."

	^self class 
		rows: numberOfRows 
		columns: numberOfColumns 
		contents: (contents collect: aBlock)