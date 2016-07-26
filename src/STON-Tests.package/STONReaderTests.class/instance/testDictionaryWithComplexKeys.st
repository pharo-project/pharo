tests
testDictionaryWithComplexKeys
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	(reader := STONReader on: '{true:1,[#foo]:2}' readStream)
		allowComplexMapKeys: true.
	self assert: reader next = collection