tests
testKeysAndValuesRemove
	| dictionary keys |

	dictionary := self dictionaryWithAssociations.
	keys := self dictionaryKeys.
	dictionary keysAndValuesRemove: [:key :value | keys first = key or: [keys third = key]].

	self
		deny: (dictionary includesKey: keys first);
		deny: (dictionary includesKey: keys third);
		assert:
			(((keys copyWithout: keys first) copyWithout: keys third)
				allSatisfy: [:each | dictionary includesKey: each]).