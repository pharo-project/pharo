invoking
invokeWithArguments: argArray
	"Manually invoke the receiver, representing an external function."
	<primitive: 'primitiveCalloutWithArgs' module:'SqueakFFIPrims'>
	^self externalCallFailed