*opalcompiler-core
isInlineRepeat
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineRepeat
		ifFalse: [ ^ false ]].
	self receiver isBlock
		ifFalse: [ ^ false ].
	self receiver arguments size isZero
		ifFalse: [ ^ false ].
	self isCascaded
		ifTrue: [ ^ false ].
	#repeat = self selector
		ifFalse: [ ^ false ].
	^ true