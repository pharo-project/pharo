assertions
assertDictionary: aFirstDictionary equals: aSecondDictionary
	"test reflixive and symmetric equality"
	self
		assert: aFirstDictionary = aFirstDictionary;
		assert: aFirstDictionary = aSecondDictionary;
		assert: aSecondDictionary = aSecondDictionary;
		assert: aSecondDictionary = aFirstDictionary