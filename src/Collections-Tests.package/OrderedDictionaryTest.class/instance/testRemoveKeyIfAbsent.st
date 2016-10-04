tests
testRemoveKeyIfAbsent
	| dictionary |

	dictionary := self dictionaryWithOrderedAssociations.
	self orderedAssociations withIndexDo: [:each :i |
		self assert:
			(dictionary
				removeKey: each key
				ifAbsent: [self fail]) = each value.
		self
			assertKey: each key
			wasRemovedfrom: dictionary.
		self
			assertIsDictionary: dictionary
			withOrderedAssociations: (self orderedAssociationsAllButFirst: i).

		self assert:
			(dictionary
				removeKey: each key
				ifAbsent: [self absentValue]) = self absentValue]