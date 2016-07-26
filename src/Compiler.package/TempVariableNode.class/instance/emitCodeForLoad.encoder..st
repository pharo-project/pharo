code generation
emitCodeForLoad: stack encoder: encoder
	remoteNode ~~ nil ifTrue:
		[remoteNode emitCodeForLoadFor: self stack: stack encoder: encoder]