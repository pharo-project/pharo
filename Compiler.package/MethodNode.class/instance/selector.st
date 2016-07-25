code generation
selector 
	"Answer the message selector for the method represented by the receiver."

	(selectorOrFalse isSymbol)
		ifTrue: [^selectorOrFalse].
	^selectorOrFalse key.
