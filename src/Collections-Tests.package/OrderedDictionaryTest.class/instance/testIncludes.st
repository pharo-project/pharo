tests
testIncludes
	| dictionary |

	dictionary := self emptyDictionary.
	self deny: (self dictionaryValues anySatisfy: [:each | dictionary includes: each]).

	dictionary addAll: self dictionaryAssociations.
	self assert: (self dictionaryValues allSatisfy: [:each | dictionary includes: each]).