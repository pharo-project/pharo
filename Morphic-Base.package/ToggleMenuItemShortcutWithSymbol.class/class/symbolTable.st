accessing
symbolTable
	^ SymbolTable ifNil: [ SymbolTable := self createSymbolTable ]