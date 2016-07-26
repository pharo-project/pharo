tests
testIndexOfKey
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	dictionary keys withIndexDo: [:each :i |
		self assert: (dictionary indexOfKey: each) = i].
	self assert: (dictionary indexOfKey: self absentKey) isZero.