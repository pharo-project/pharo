*UnifiedFFI
callbackEvaluator: anEvaluator  
	evaluator := #evaluatorValueWithContext:sp:.
	numEvaluatorArgs := 2.
	block := anEvaluator.
	thunk := FFICallbackThunk newCCall.
	self addToThunkTable