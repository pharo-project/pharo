code generation
emitValue: aMethodBuilder
	"Push the process local variable into the stack"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder send: #value