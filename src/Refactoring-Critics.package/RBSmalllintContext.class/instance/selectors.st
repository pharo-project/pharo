accessing
selectors
	literalSemaphore isNil
		ifTrue: 
			[selectors isNil ifTrue: 
					[self computeLiterals.
					literalSemaphore wait]]
		ifFalse: [literalSemaphore wait].
	^selectors