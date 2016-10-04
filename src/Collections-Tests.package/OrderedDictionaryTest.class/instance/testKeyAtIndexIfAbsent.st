tests
testKeyAtIndexIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self assert:
		(dictionary
			keyAtIndex: 0
			ifAbsent: [self absentKey]) = self absentKey.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				keyAtIndex: i
				ifAbsent: [self absentKey]) = self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtIndex: i
				ifAbsent: [self fail]) = each key].