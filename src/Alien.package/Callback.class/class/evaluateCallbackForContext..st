evaluation
evaluateCallbackForContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<FFICallbackReturnValue>"
	(ThunkToCallbackMap
		at: callbackContext thunkp
		ifAbsent: [^self error: 'could not locate Callback instance corresponding to thunk address'])
		ifNil: [self error: 'Callback instance for this thunk address has been garbage collected']
		ifNotNil:
			[:callback|
			^callback valueInContext: callbackContext]