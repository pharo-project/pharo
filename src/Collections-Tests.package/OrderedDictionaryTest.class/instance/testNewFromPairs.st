tests
testNewFromPairs
	| pairs |

	pairs := OrderedCollection new.
	self dictionaryAssociations do: [:each |
		pairs
			add: each key;
			add: each value].
	0 to: pairs size do: [:i |
		self
			assertDictionary:
				(self dictionaryClass newFromPairs: (pairs copyFrom: 1 to: i))
			equals:
				(self dictionaryClass newFrom:
					(self dictionaryAssociations copyFrom: 1 to: (i / 2) floor))].