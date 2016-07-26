code generation
emitCodeForStorePop: stack encoder: encoder
	writeNode ifNil:
		[stack pop: 1.
		 ^encoder genStorePopLiteralVar: index].
	self emitCodeForStore: stack encoder: encoder.
	encoder genPop.
	stack pop: 1.