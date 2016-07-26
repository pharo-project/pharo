tests
testAddAll
	| dictionary |

	dictionary := self emptyDictionary.
	self deny:
		(self dictionaryAssociations anySatisfy: [:each |
			dictionary includesAssociation: each]).

	self
		assert: (dictionary addAll: self dictionaryAssociations) = self dictionaryAssociations;
		assert: dictionary associations = self dictionaryAssociations.

	"ensure adding the same associations doesn't change the order"
	self assert:
		(dictionary addAll: self dictionaryAssociations reversed) =
			self dictionaryAssociations reversed.
	self assertIsDictionary: dictionary withAssociations: self dictionaryAssociations.