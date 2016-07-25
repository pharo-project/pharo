code generation
sizeCodeForLoad: encoder
	^remoteNode
		ifNil: [0]
		ifNotNil: [remoteNode sizeCodeForLoadFor: self encoder: encoder]