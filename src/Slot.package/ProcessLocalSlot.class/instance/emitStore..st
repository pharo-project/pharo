code generation
emitStore: aMethodBuilder
	| temp |
	temp := Object new.
	"We pop the value from the stack into a temp to push it back in the right order"
	aMethodBuilder addTemp: temp.
	aMethodBuilder storeTemp: temp.
	
	"Push the process local variable into the stack, then the value again, then send"
	aMethodBuilder pushInstVar: index.
	aMethodBuilder pushTemp: temp.
	aMethodBuilder send: #value: