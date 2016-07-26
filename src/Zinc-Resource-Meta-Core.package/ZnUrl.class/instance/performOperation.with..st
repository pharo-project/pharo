operations
performOperation: operation with: argument
	"Look for and execute a handler that can perform operation 
	on the receiver with the optional argument"
	
	^ ZnUrlOperation
		performOperation: operation 
		with: argument 
		on: self 