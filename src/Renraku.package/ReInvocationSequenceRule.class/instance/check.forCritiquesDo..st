running
check: aNode forCritiquesDo: aCriticBlock

	aNode isMethod ifFalse: [ ^ self ].
	
	(self  preCheck: aNode forCriticsDo: aCriticBlock) &  
	(self postCheck: aNode forCriticsDo: aCriticBlock)