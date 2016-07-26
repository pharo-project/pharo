code generation
emitCodeForStorePop: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStorePopInto: self stack: stack encoder: encoder].
	encoder genStorePopTemp: index.
	stack pop: 1