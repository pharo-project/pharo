tests
testOccurancesOf
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: (dictionary occurrencesOf: self absentValue) isZero.

	dictionary add: (self dictionaryAssociations first value: self newValue).
	self assert: (dictionary occurrencesOf: self newValue) = 1.

	dictionary add: (self dictionaryAssociations second value: self newValue) .
	self assert: (dictionary occurrencesOf: self newValue) = 2.