accessing
literals
	literalSemaphore isNil
		ifTrue: 
			[literals isNil ifTrue: 
					[self computeLiterals.
					literalSemaphore wait]]
		ifFalse: [literalSemaphore wait].
	^literals