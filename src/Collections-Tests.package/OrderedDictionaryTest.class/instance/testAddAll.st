tests
testAddAll
	| dictionary addedAssociations |

	dictionary := self emptyDictionary.
	self deny:
		(self dictionaryAssociations anySatisfy: [:each |
			dictionary includesAssociation: each]).

	addedAssociations := self dictionaryAssociations.
	self assert: (dictionary addAll: addedAssociations) == addedAssociations.
	self
		assertIsDictionary: dictionary
		withAssociations: addedAssociations.

	"ensure adding the same associations doesn't change the order"
	addedAssociations := self dictionaryAssociations reversed.
	self assert: (dictionary addAll: addedAssociations) == addedAssociations.
	self
		assertIsDictionary: dictionary
		withAssociations: self dictionaryAssociations.