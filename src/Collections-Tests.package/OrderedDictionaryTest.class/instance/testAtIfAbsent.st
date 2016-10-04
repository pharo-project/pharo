tests
testAtIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				at: each key
				ifAbsent: [self absentValue]) = self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				at: each key
				ifAbsent: [self fail]) = each value].