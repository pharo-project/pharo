running
check: aNode forCritiquesDo: aCriticBlock

	aNode isLiteralArray ifFalse: [ ^ self ].
	(aNode value includesAny: #(#true #false #nil))
		ifFalse: [ ^ self ].
		
	aCriticBlock cull: (self critiqueFor: aNode) 