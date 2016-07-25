code generation
emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVarLong: index.
	stack pop: 1