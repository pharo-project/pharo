running
check: aNode forCritiquesDo: aCriticBlock

	aNode isLiteralArray ifFalse: [ ^ self ].
	aNode value ifEmpty: [ ^ self ].
	(aNode value allSatisfy: #isCharacter)
		ifTrue: [ 
			aCriticBlock cull: (self critiqueFor: aNode) ]