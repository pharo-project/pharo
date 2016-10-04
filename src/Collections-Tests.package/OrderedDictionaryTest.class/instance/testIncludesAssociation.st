tests
testIncludesAssociation
	| dictionary |

	dictionary := self emptyDictionary.
	self orderedAssociations do: [:each |
		self
			deny: (dictionary includesAssociation: each);
			deny: (dictionary includesAssociation: each key -> each value).

		dictionary add: each.
		self
			assert: (dictionary includesAssociation: each);
			assert: (dictionary includesAssociation: each key -> each value)].