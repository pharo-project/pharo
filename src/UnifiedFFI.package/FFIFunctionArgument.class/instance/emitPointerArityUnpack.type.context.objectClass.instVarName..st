emitting code
emitPointerArityUnpack: aBuilder type: aType context: aContext objectClass: objectClass instVarName: aName
	aType needsArityUnpacking ifFalse: [ ^ self ].
	self 
		emitPointerArityUnpack: aBuilder 
		context: aContext 
		objectClass: objectClass 
		instVarName: aName
		arity: aType pointerArity