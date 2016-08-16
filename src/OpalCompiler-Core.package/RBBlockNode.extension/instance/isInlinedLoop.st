*opalcompiler-core
isInlinedLoop
	parent isMessage ifFalse: [ ^ false ].
	parent isInlineToDo ifTrue: [^ true].
	parent isInlineWhile ifTrue: [^ true].
	parent isInlineTimesRepeat ifTrue: [^ true].
	parent isInlineRepeat ifTrue: [ ^true ].
	(parent parent isDynamicArray and: [parent parent parent isMessage and: [parent parent parent isInlineCase]]) ifTrue: [^ true].
	parent isInlineCase ifTrue: [^ true].  "otherwise branch"
	^ false