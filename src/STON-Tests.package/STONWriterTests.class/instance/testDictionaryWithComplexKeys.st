tests
testDictionaryWithComplexKeys
	| collection |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	
	self assert: (#('{true:1,[#foo]:2}' '{[#foo]:2,true:1}') includes: (self serialize: collection)).