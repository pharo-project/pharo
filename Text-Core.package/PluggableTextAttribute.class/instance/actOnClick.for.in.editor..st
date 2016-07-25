evaluating
actOnClick: anEvent for: anObject in: paragraph editor: editor

	evalBlock ifNil: [ ^self ].
	evalBlock numArgs = 0 ifTrue: [ evalBlock value.  ^true ].
	evalBlock numArgs = 1 ifTrue: [ evalBlock value: anObject.  ^true ].
	self error: 'evalBlock should have 0 or 1 arguments'