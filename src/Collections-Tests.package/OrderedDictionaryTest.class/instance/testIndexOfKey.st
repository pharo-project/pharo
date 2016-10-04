tests
testIndexOfKey
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert: (dictionary indexOfKey: each key) = 0.

		dictionary add: each.
		self assert: (dictionary indexOfKey: each key) = i].