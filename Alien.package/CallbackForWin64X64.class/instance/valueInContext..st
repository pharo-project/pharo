evaluation
valueInContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	numEvaluatorArgs = 2 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp].
	numEvaluatorArgs = 3 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp
			with: callbackContext stackp].
	numEvaluatorArgs = 4 ifTrue:
		[^self perform: evaluator
			with: callbackContext
			with: callbackContext intregargsp
			with: callbackContext stackp
			with: callbackContext floatregargsp].
	numEvaluatorArgs = 1 ifTrue: "legacy support"
		[^self perform: evaluator
			with: callbackContext].
	self error: 'invalid evaluator argument count'