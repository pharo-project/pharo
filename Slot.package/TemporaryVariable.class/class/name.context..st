instance creation
name: aName context: aContext
	^aContext isBlock
		ifTrue: [ self name: aName block: aContext closure ]
		ifFalse: [ self name: aName method: aContext method ]