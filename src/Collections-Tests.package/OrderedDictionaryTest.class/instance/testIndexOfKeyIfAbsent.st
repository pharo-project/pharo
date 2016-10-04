tests
testIndexOfKeyIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				indexOfKey: each key
				ifAbsent: [self absentValue]) = self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				indexOfKey: each key
				ifAbsent: [self fail]) = i].