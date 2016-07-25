emitting code
emitPointerArityPack: aBuilder context: aContext arity: arity selector: aSelector
	"I take into account complex arities (like **, ***). 
	 I'm called right after argument marshaling which means it is already in top of the stack"
	| ptrName |
	ptrName := self rolledPointerTempName.
	aBuilder 
		addTemp: ptrName;
		pushLiteral: arity;
		send: aSelector;
		storeTemp: ptrName