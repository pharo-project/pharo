code generation
emitCodeForLoad: stack encoder: encoder
	writeNode ifNotNil:
		[encoder genPushLiteral: index.
		 stack push: 1]