private-exceptions
evaluateSignal: exception
	"The following primitive is just a marker used to find the evaluation context. 
	See MethodContext>>#isHandlerOrSignalingContext. "

	<primitive: 199>
	| value |
	exception privHandlerContext: self contextTag.
	value := self exceptionHandlerBlock cull: exception.	
	"return from self if not otherwise directed in handle block"
	self return: value