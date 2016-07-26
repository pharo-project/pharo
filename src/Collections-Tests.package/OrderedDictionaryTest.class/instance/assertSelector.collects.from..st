assertions
assertSelector: aSelector collects: aCollection from: aDictionary
	self
		assert: (self emptyDictionary perform: aSelector) asArray = #();
		assert: (aDictionary perform: aSelector) asArray = aCollection asArray