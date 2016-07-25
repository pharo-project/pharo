link api
condition: aCondition
	condition:= aCondition.
	self optionInlineCondition ifTrue: [^self invalidate].
	nodes do: [ :node |  | methodNode |
		methodNode := node methodNode.
		(methodNode metaLinkOptionsFromClassAndMethod includes: #optionInlineCondition)
			 ifTrue: [ methodNode method invalidate ] ].
	

	
