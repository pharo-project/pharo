tests
testAssociationAtIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				associationAt: each key
				ifAbsent: [self absentValue]) = self absentValue.

		dictionary add: each.
		self assert:
			(dictionary
				associationAt: each key
				ifAbsent: [self fail]) = each].