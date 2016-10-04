tests
testKeys
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary keys
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		self
			assertIsArray: dictionary keys
			withElements: (self orderedKeysFirst: i)].