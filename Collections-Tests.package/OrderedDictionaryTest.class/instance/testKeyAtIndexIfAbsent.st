tests
testKeyAtIndexIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assert: (dictionary keyAtIndex: 0 ifAbsent: [self newKey]) = self newKey;
		assert: (dictionary keyAtIndex: 1 ifAbsent: [self newKey]) = self newKey.

	dictionary := self dictionaryWithAssociations.
	self dictionaryKeys withIndexDo: [:each :i |
		self assert: (dictionary keyAtIndex: i ifAbsent: [self newKey]) = each].
	self assert:
		(dictionary
			keyAtIndex: 0
			ifAbsent: [self newKey]) = self newKey.
	self assert:
			(dictionary
				keyAtIndex: dictionary size + 1
				ifAbsent: [self newKey]) = self newKey.