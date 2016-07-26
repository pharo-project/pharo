visitor-double dispatching
emitMessageNode: aMessageNode
	self emitPrepareLinkAfter: aMessageNode.
	aMessageNode isCascaded
		ifFalse: [ valueTranslator visitNode: aMessageNode receiver ].
	aMessageNode arguments do: [ :each | valueTranslator visitNode: each ].
	self emitMetaLinkBefore: aMessageNode.
	aMessageNode hasMetalinkInstead
		ifTrue: [ self emitMetaLinkInstead: aMessageNode ]
		ifFalse: [ 
			aMessageNode isSuperSend
				ifTrue: [ methodBuilder send: aMessageNode selector toSuperOf: self compilationContext getClass ]
				ifFalse: [ methodBuilder send: aMessageNode selector ] ].
	self emitMetaLinkAfter: aMessageNode.