emitting code
emitPointerArityPack: aBuilder context: aContext arity: arity
	"I take into account complex arities (like **, ***). 
	 I'm called right after argument marshaling which means it is already in top of the stack"
	^ self 
		emitPointerArityPack: aBuilder 
		context: aContext 
		arity: arity
		selector: #packToArity: