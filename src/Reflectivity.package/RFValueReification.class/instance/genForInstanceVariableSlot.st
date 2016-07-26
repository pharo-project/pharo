generate
genForInstanceVariableSlot
	"entity in this case is the binding of the instance variable"
	^RBMessageNode receiver: entity asLiteralNode selector: #read: arguments: { RBVariableNode named: 'self' }