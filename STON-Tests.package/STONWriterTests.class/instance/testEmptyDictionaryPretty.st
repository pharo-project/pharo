tests
testEmptyDictionaryPretty
	self assert: (self serializePretty: STON mapClass new) equals: '{ }' 