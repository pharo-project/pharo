private
shutDown: aboutToQuit

	SymbolTable addAll: NewSymbols.
	NewSymbols := WeakSet new.
	aboutToQuit ifTrue: [self resetSelectorTable].