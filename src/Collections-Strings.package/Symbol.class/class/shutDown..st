private
shutDown: aboutToQuit

	SymbolTable addAll: NewSymbols.
	NewSymbols := WeakSet new.
