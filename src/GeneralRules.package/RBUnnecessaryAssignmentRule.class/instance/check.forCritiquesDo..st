running
check: aNode forCritiquesDo: aCriticBlock
	
	aNode isReturn ifFalse: [ ^ self ].
	aNode isAssignment ifFalse: [ ^ self ].
	(aNode whoDefines: aNode variable name) ifNil: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aNode)