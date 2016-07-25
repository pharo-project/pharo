managing elements groups
elementsCategorized: aSymbol
	"Retrieves a group of elements.
	If does not exist set an Set"
	
	^elements at: aSymbol ifAbsentPut:[ Set ]