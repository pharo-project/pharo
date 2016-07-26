tests
testEmptyArrayPretty
	self assert: (self serializePretty: STON listClass new) equals: '[ ]' 