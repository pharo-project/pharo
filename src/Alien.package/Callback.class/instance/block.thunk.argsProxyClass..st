evaluation
block: aBlock "<BlockClosure>" thunk: thunkWrapper "<FFICallbackThunk>" argsProxyClass: argsWrapperClass "<Alien class>"
	<legacy>
	block := aBlock.
	thunk := thunkWrapper.
	argsProxyClass := argsWrapperClass.
	numEvaluatorArgs := 1.
	evaluator := #valueWithContext:.
	self addToThunkTable