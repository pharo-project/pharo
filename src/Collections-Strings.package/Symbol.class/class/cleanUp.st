cleanup
cleanUp
	"Flush caches"

	self compactSymbolTable.
	self rebuildSelectorTable