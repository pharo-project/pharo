tests
testAtPut
	| dictionary keyToUpdate oldValue |

	dictionary := self emptyDictionary.
	self assert:
		(self dictionaryAssociations allSatisfy: [:each |
			(dictionary at: each key put: each value) = each value]).
	self assert: dictionary associations = self dictionaryAssociations.

	keyToUpdate := self dictionaryKeys anyOne.
	oldValue := dictionary at: keyToUpdate.
	self
		assert: (dictionary at: keyToUpdate put: self newValue) = self newValue;
		assert: (dictionary at: keyToUpdate) = self newValue;
		assert:
			(dictionary associations =
				(self dictionaryAssociations collect: [:each |
					(each value = oldValue)
						ifTrue: [each key -> self newValue]
						ifFalse: [each]])).