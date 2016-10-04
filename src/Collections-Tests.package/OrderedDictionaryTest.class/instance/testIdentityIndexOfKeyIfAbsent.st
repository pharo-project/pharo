tests
testIdentityIndexOfKeyIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				identityIndexOfKey: each key
				ifAbsent: [self absentValue]) = self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				identityIndexOfKey: each key
				ifAbsent: [self fail]) = i.
		self assert:
			(dictionary
				identityIndexOfKey: each key copy
				ifAbsent: [self absentValue]) = self absentValue].