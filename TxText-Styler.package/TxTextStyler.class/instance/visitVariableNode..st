visiting
visitVariableNode: aVariableNode 
"	aVariableNode name first isUppercase 
		ifTrue: [environment ifNil: [ ^ self visitClassReferenceNode: aVariableNode ]].
"
	| link |
	
	self 
		addStyle: (self resolveStyleFor: aVariableNode) 
		forNode: aVariableNode.
	
	(aVariableNode binding notNil and: [  
	aVariableNode isGlobal ]) ifTrue: [  
		link := TxActionAttribute new 
			filter: [ :evt | evt commandKeyPressed and: [ evt  isMouseDown] ];
			value: [ aVariableNode binding value instanceSide browse ];
			yourself.

		self 
			addAttribute: link
			forNode: aVariableNode.
	]