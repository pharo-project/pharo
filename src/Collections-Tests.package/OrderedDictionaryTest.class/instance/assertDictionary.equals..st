assertions
assertDictionary: aDictionary equals: aSecondDictionary
	self
		assert: aDictionary = aDictionary;
		assert: aSecondDictionary = aSecondDictionary;
		assert: aDictionary = aSecondDictionary;
		assert: aSecondDictionary = aDictionary