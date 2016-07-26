code generation
emitValue: aMethodBuilder
	"Push the weak array into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder send: #at: