callbacks
invokeCallback: thunkPtr "<Integer>" stack: stackPtr "<Integer>" registers: regsPtr "<Integer>" jmpbuf: jmpBufPtr "<Integer>" "^<FFICallbackReturnValue>"
	"The low-level entry-point for callbacks sent from the VM/IA32ABI plugin.
	 Return via primReturnFromContext:through:.  thisContext's sender is the
	 call-out context."
	| resultProxy |
	[resultProxy := Callback evaluateCallbackForThunk: thunkPtr stack: stackPtr]
		ifCurtailed: [self error: 'attempt to non-local return across a callback'].
	resultProxy ifNil:
		[(resultProxy := FFICallbackReturnValue new) returnInteger: 0].
	resultProxy primReturnFromContext: thisContext through: jmpBufPtr