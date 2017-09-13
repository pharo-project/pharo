running
check: aNode forCritiquesDo: aCritiqueBlock 
	aNode isLiteralArray ifFalse: [ ^ self ].
	(aNode value includes: #,) ifFalse: [ ^ self ].


	aCritiqueBlock cull: (self critiqueFor: aNode)