code generation
emitCodeForStorePop: stack encoder: encoder
	self assert: [self type ~= 1].
	encoder genStorePopInstVar: index.
	stack pop: 1