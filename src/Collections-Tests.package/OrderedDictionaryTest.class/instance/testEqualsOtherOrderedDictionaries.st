tests
testEqualsOtherOrderedDictionaries
	self otherOrderedDictionaryClasses do: [:each |
		self
			assertDictionary: self emptyDictionary
			doesNotEqual: each new.
		self
			assertDictionary: self dictionaryWithOrderedAssociations
			doesNotEqual: (each newFrom: self orderedAssociations)]