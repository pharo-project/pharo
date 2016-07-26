evaluation
valueWithStackPointer: stackPtr "<Integer>" "^<FFICallbackReturnValue>"
	<legacy>
	| resultProxy |
	block
		value: (argsProxyClass atAddress: stackPtr)
		value: (resultProxy := FFICallbackReturnValue new).
	^resultProxy