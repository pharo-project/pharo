code generation
emitCodeForValue: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[^remoteNode emitCodeForValueOf: self stack: stack encoder: encoder].
	encoder genPushTemp: index.
	stack push: 1