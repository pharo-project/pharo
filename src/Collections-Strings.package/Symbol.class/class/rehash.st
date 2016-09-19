private
rehash
   "Symbol rehash"
	"Rebuild the hash table, reclaiming unreferenced Symbols."

	SymbolTable := WeakSet withAll: self allSubInstances.
	NewSymbols := WeakSet new.
	self rebuildSelectorTable.