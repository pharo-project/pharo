tests
testEvaluateWithBindingsWithUppercaseName
	| result |
	result := Smalltalk compiler
		bindings: { #MyVar -> 3 } asDictionary;
		evaluate: '1+MyVar'.
		
	self assert: result = 4.