assertions
assertDictionary: aDictionary doesNotEqual: aSecondDictionary
	self
		deny: aDictionary = aSecondDictionary;
		deny: aSecondDictionary = aDictionary