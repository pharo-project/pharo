code generation
emitCodeForStore: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForStoreInto: self stack: stack encoder: encoder].
	encoder genStoreTemp: index