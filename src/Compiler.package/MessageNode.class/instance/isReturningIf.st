testing
isReturningIf

	^((special between: 3 and: 4) "ifTrue:ifFalse:/ifFalse:ifTrue:"
	    or: [special between: 17 and: 18]) "ifNil:ifNotNil:/ifNotNil:ifNil:"
		and: [arguments first returns and: [arguments last returns]]