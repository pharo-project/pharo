running
check: aNode forCritiquesDo: aCriticBlock

	((self checkIsVariableWithParent: aNode) and: [ 
	
	((self checkIsArgumentDefinition: aNode) or: [ 
	  self checkIsTempDefinition: aNode ])
	
	and: [ aNode name first isUppercase ] ]) 

	ifTrue: [ aCriticBlock cull: (self critiqueFor: aNode) ]