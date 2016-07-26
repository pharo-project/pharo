printing
printKeywords: key arguments: args on: aStream indent: level
	| keywords indent arg kwd doCrTab |
	args size = 0 ifTrue:
		[aStream space; nextPutAll: key.
		 ^self].
	keywords := key keywords.
	doCrTab := args size > 2
				or: [{receiver} , args anySatisfy:
						[:thisArg |
						thisArg notNil
						and: [thisArg isBlockNode
							 or: [thisArg isMessageNode and: [thisArg precedence >= 3]]]]].
	1 to: (args size min: keywords size) do:
		[:i |
		arg := args at: i.
		kwd := keywords at: i.
		doCrTab
			ifTrue: [aStream crtab: level+1. indent := 1] "newline after big args"
			ifFalse: [aStream space. indent := 0].
		aStream nextPutAll: kwd; space.
		arg printOn: aStream
			indent: level + 1 + indent
			precedence: (precedence = 2 ifTrue: [1] ifFalse: [precedence])]