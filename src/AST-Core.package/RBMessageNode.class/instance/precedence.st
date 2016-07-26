accessing
precedence
	^self isUnary 
		ifTrue: [1]
		ifFalse: [self isKeyword ifTrue: [3] ifFalse: [2]]