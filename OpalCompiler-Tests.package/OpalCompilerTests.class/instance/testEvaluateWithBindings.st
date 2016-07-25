tests
testEvaluateWithBindings
	| result |
	result := Smalltalk compiler
		bindings: { #a -> 3 } asDictionary;
		evaluate: '1+a'.
		
	self assert: result = 4.