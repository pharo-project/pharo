invoking
tryInvokeWithArguments: argArray
	"Sent from the debugger to simulate an FFI call."
	<primitive: 'primitiveCalloutWithArgs' module: 'SqueakFFIPrims' error: ec>
	
	^thisContext class primitiveFailTokenFor: ec