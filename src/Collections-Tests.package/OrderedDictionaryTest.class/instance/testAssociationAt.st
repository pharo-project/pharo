tests
testAssociationAt
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary associationAt: each key]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary associationAt: each key) = each].