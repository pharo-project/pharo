tests
testKeyAtValueIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				keyAtValue: each value
				ifAbsent: [self absentKey]) = self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtValue: each value
				ifAbsent: [self fail]) = each key].