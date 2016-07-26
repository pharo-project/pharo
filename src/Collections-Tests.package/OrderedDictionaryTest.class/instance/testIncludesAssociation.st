tests
testIncludesAssociation
	| dictionary |

	dictionary := self emptyDictionary.
	self deny: (self dictionaryAssociations anySatisfy: [:each | dictionary includesAssociation: each]).

	dictionary addAll: self dictionaryAssociations.
	self assert: (self dictionaryAssociations allSatisfy: [:each | dictionary includesAssociation: each])