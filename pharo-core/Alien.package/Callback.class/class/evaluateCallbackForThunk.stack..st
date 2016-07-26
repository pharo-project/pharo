evaluation
evaluateCallbackForThunk: thunkPtr "<Integer>" stack: stackPtr "<Integer>" "^<FFICallbackReturnValue>"
	<legacy>
	| callback |
	callback := ThunkToCallbackMap
		at: thunkPtr
		ifAbsent: [^self error: 'could not locate Callback instance corresponding to thunk address'].
	callback isNil
		ifTrue: [self error: 'Callback instance for this thunk address has been garbage collected']
		ifFalse: [^callback valueWithStackPointer: stackPtr]