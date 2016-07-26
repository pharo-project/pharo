exceptions
on: exception do: handlerAction
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value