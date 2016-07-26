accessing instances and variables
bindingOf: varName
	
	"Answer the binding of some variable resolved in the scope of the receiver"

	^ self environment bindingOf: varName asSymbol.