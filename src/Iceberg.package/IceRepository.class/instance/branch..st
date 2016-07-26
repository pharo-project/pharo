accessing
branch: anObject
	branch := anObject.
	repository ifNotNil: [ repository branch: branch ]