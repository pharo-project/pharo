assertions
assertDictionary: aFirstDictionary doesNotEqual: aSecondDictionary
	"test symmetric inequality"
	self
		deny: aFirstDictionary = aSecondDictionary;
		deny: aSecondDictionary = aFirstDictionary