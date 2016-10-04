tests
testKeyForIdentity
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self assert: (dictionary keyForIdentity: each value) isNil.

		dictionary add: each.
		self
			assert: (dictionary keyForIdentity: each value) = each key;
			assert: (dictionary keyForIdentity: each value copy) isNil].