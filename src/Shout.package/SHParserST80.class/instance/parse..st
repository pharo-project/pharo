parse
parse: aBoolean 
	self initializeInstanceVariables.
	sourcePosition := 1.
	arguments := Dictionary new.
	temporaries := Dictionary new.
	blockDepth := bracketDepth := 0.
	ranges isNil
		ifTrue: [ranges := OrderedCollection new: 100]
		ifFalse: [ranges reset].
	errorBlock := [^ false].
	[self scanNext.
	aBoolean
		ifTrue: [self parseMessagePattern.
			self parsePragmaSequence].
	self parseMethodTemporaries.
	aBoolean
		ifTrue: [self parsePragmaSequence].
	self parseStatementList.
	currentToken
		ifNotNil: [self error]]
		ensure: [errorBlock := nil].
	^ true