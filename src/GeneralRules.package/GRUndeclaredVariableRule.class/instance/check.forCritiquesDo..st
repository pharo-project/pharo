running
check: aNode forCritiquesDo: aCriticBlock
	
	(aNode isVariable and: [
	 aNode isUndeclared ]) ifTrue: [ 
		aCriticBlock value: (
			self critiqueFor: aNode) ]