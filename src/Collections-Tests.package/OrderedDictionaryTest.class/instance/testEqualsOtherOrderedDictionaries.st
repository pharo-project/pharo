tests
testEqualsOtherOrderedDictionaries
	self otherOrderedDictionaryClasses do: [:each |
		self
			assertDictionary: self emptyDictionary
			doesNotEqual: each new.
		self
			assertDictionary: self dictionaryWithAssociations
			doesNotEqual: (each newFrom: self dictionaryAssociations)]