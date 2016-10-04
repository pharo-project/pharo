tests
testKeyAtValue
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			should: [dictionary keyAtValue: each value]
			raise: Error.

		dictionary add: each.
		self assert: (dictionary keyAtValue: each value) = each key].