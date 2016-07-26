tests
testIncludesIdentity
	| dictionary associations values |

	dictionary := self emptyDictionary.

	"Need to save references to the exact objects for identity tests"
	associations := self dictionaryAssociations.
	values := associations collect: [:each | each value].
	self deny: (values anySatisfy: [:each | dictionary includesIdentity: each ]).

	dictionary addAll: associations.
	self
		assert: (values allSatisfy: [:each | dictionary includesIdentity: each]);
		deny: (values anySatisfy: [:each | dictionary includesIdentity: each copy]).