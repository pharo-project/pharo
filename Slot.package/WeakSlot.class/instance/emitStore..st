code generation
emitStore: aMethodBuilder
	| temp |
	temp := Object new.
	"Pop the value to store into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	
	"Push the weak array into the stack, then the arguments, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushLiteral: 1.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #at:put: