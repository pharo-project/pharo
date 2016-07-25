tests - instance creation
testIsSelfEvaluating
	| array array2 |
	array := RunArray new: 5 withAll: 2.
	self assert: array isSelfEvaluating.
	array2 := RunArray new: 5 withAll: Object new.
	array2 isSelfEvaluating ifTrue: [self
		assert: (self class compiler evaluate: array2 printString) = array2
		description: 'a self evaluating should evaluate as self'].