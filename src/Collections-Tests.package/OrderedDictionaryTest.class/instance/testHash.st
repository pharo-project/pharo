tests
testHash
	| dictionary otherDictionary internalDictionary otherInternalDictionary |

	dictionary := self emptyDictionary.
	otherDictionary := self emptyDictionary.
	internalDictionary := self emptyInternalDictionary.
	otherInternalDictionary := self emptyInternalDictionary.
	self assert: dictionary hash = otherDictionary hash.
	self dictionaryAssociations do: [:each |
		dictionary add: each.
		internalDictionary add: each copy.
		"if the internal hashes differ after adding to one, the external should too"
		internalDictionary hash = otherInternalDictionary hash
			ifFalse: [self deny: dictionary hash = otherDictionary hash].

		otherDictionary add: each copy.
		otherInternalDictionary add: each copy.
		"should be equal regardless now"
		self assert: dictionary hash = otherDictionary hash].