tests
testKeyAtIdentityValue
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary keyAtIdentityValue: each value]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtIdentityValue: each value) = each key.
		self
			should: [dictionary keyAtIdentityValue: each value copy]
			raise: Error].