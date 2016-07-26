code generation
emitCodeForStorePop: stack encoder: encoder
	encoder genStorePopInstVar: index.
	stack pop: 1