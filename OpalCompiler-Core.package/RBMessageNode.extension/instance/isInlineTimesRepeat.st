*opalcompiler-core
isInlineTimesRepeat

	| block |
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineTimesRepeat ifFalse: [ ^false ]]. 

	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	#timesRepeat: = self selector ifFalse: [^ false].
	block := self arguments last.
	block isBlock ifFalse: [^ false].
	block arguments size isZero ifFalse: [^ false].
	^ true