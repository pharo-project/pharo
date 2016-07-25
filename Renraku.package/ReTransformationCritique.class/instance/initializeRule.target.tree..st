initialization
initializeRule: aRule target: anEntity tree: newAST

	self initializeRule: aRule target: anEntity.
	newTree := newAST 