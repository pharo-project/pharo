running
check: anAST forCritiquesDo: aCriticBlock
	
	(self  preCheck: anAST forCriticsDo: aCriticBlock) &  
	(self postCheck: anAST forCriticsDo: aCriticBlock)