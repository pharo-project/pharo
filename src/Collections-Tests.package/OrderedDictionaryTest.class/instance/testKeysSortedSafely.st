tests
testKeysSortedSafely
	| dictionary |

	dictionary := self emptyDictionary.
	self
		assertIsArray: dictionary keysSortedSafely
		withElements: #().
	self orderedAssociations withIndexDo: [:each :i |
		dictionary add: each.
		"The keys are all strings, so the default #sorted implementation is OK."
		self
			assertIsArray: dictionary keysSortedSafely
			withElements: (self orderedKeysFirst: i) sorted].