accessing
superMessages
	superMessages isNil ifTrue: [self computeMessages].
	^superMessages