tests
testIncludesIdentity
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self deny: (dictionary includesIdentity: each value).

		dictionary add: each.
		self
			assert: (dictionary includesIdentity: each value);
			deny: (dictionary includesIdentity: each value copy)].