emitting code
emitReturnArgument: builder context: aContext
	self loader 
		emitPointerArityUnpack: builder
		type: self
		context: aContext
		objectClass: self objectClass
		instVarName: self instanceVariableName