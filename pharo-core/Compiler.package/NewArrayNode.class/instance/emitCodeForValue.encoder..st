code generation
emitCodeForValue: stack encoder: encoder
	encoder genPushNewArray: numElements.
	stack push: 1