message performing
withReceiver: receiver tryPrimitive: number withArguments: args
	"This method is a template that the Smalltalk simulator uses to 
	execute primitives"

	<primitive: 118 error: code >
	^ Context primitiveFailTokenFor: code.	