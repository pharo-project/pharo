managing elements groups
removeElementsCategorized: aSymbol
	"Deletes a group of elements"
	
	elements removeKey: aSymbol ifAbsent:[ ]