tests
testDictionaryWithComplexKeys
	| collection reader |
	collection := STON mapClass new at: true put: 1; at: #(foo) put: 2; yourself.
	"allowing complex map keys used to be optional, now it is always the default"
	reader := STONReader on: '{true:1,[#foo]:2}' readStream.
	self assert: reader next = collection