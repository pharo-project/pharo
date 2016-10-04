tests
testIdentityIndexOfKey
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert: (dictionary identityIndexOfKey: each key) = 0.

		dictionary add: each.
		self
			assert: (dictionary identityIndexOfKey: each key) = i;
			assert: (dictionary identityIndexOfKey: each key copy) = 0].