tests
testOccurancesOf
	| dictionary |

	dictionary := self emptyDictionary.
	self assert: (dictionary occurrencesOf: self newValue) = 0.
	self orderedKeys withIndexDo: [:each :i |
		dictionary
			at: each
			put: self newValue.
		self assert: (dictionary occurrencesOf: self newValue) = i]