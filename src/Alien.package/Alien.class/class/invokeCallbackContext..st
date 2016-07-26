callbacks
invokeCallbackContext: vmCallbackContextAddress "<Integer>" "^<FFICallbackReturnValue>"
	"The low-level entry-point for callbacks sent from the VM/IA32ABI plugin.
	 Return via primReturnFromContext:through:.  thisContext's sender is the
	 call-out context."
	| callbackAlien type |
	callbackAlien := (Smalltalk wordSize = 4
						ifTrue: [VMCallbackContext32]
						ifFalse: [VMCallbackContext64])
							atAddress: vmCallbackContextAddress.
	[type := Callback evaluateCallbackForContext: callbackAlien]
		ifCurtailed: [self error: 'attempt to non-local return across a callback'].
	type ifNil:
		[type := 1. callbackAlien wordResult: -1].
	callbackAlien primReturnAs: type fromContext: thisContext