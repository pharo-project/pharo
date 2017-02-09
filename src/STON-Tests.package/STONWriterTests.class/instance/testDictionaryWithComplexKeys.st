tests
testDictionaryWithComplexKeys
	| collection |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	self assert: (self serialize: collection) = '{true:1,[#foo]:2}'.