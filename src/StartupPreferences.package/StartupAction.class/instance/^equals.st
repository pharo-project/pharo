accessing
= anObject

	anObject class = self class ifFalse: [ ^ false ].
	^ self code = anObject code and: [ self name = anObject name and: [ self runOnce = anObject runOnce ]]