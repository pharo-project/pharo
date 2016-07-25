link api
metaObject: anObject
	metaObject := anObject.
	self optionInlineMetaObject ifTrue: [^self invalidate].
	nodes do: [ :node | | methodNode |
		methodNode := node methodNode.
		(methodNode metaLinkOptionsFromClassAndMethod includes: #optionInlineMetaObject)
			 ifTrue: [ methodNode method invalidate ] ].
	
	