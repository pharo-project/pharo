*opalcompiler-core
isInlined 
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineNone ifTrue: [ ^false ]]. 
	self isInlineIf ifTrue: [^true].
	self isInlineIfNil  ifTrue: [^true]. 
	self isInlineAndOr  ifTrue: [^true].
	self isInlineWhile ifTrue: [^true].
	self isInlineToDo  ifTrue: [^true].
	self isInlineCase  ifTrue: [^true].
	self isInlineTimesRepeat ifTrue: [^true].
	self isInlineRepeat ifTrue: [^true].
	^false.