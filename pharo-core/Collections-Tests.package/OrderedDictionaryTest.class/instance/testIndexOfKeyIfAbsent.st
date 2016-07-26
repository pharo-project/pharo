tests
testIndexOfKeyIfAbsent
	| dictionary |

	dictionary := self dictionaryWithAssociations.
	dictionary keys withIndexDo: [:each :i |
		self assert: (dictionary indexOfKey: each ifAbsent: [self absentValue]) = i].

	self assert:
		(dictionary
			indexOfKey: self absentKey
			ifAbsent: [self absentValue]) = self absentValue.