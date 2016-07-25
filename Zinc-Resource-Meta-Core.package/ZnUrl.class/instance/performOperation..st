operations
performOperation: operation
	"Look for and execute a handler that can perform operation on the receiver"
	
	^ self performOperation: operation with: nil