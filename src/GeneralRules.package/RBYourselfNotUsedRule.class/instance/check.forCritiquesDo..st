enumerating
check: aNode forCritiquesDo: aCriticBlock
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #yourself ifFalse: [ ^ self ].
	aNode isUsed ifFalse: [ 
		aCriticBlock cull: (
			self critiqueFor: aNode) ]