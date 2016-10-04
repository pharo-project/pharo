tests
testKeyAtIdentityValueIfAbsent
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert:
			(dictionary
				keyAtIdentityValue: each value
				ifAbsent: [self absentKey]) = self absentKey.

		dictionary add: each.
		self assert:
			(dictionary
				keyAtIdentityValue: each value
				ifAbsent: [self fail]) = each key.
		self assert:
			(dictionary
				keyAtIdentityValue: each value copy
				ifAbsent: [self absentKey]) = self absentKey].