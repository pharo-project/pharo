tests
testAtIfPresentIfAbsent
	| dictionary |

	dictionary := self dictionaryWithSingleAssociation.
	self assert:
		(dictionary
			at: self singleKey
			ifPresent: [:value |
				value = self singleValue
					ifTrue: [self newValue]
					ifFalse: [nil]]
			ifAbsent: [self signalFailure: 'Block should not have been evaluated']) = self newValue.
	self assert:
		(dictionary
			at: self absentKey
			ifPresent: [self signalFailure: 'Block should not have been evaluated']
			ifAbsent: [self newValue]) = self newValue.