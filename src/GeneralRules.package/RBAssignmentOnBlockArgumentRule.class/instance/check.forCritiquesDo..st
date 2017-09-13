running
check: aMethod forCritiquesDo: aCriticBlock
	((RBFindBlockArgumentAssignment collectAssignments: aMethod ast) notEmpty) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aMethod) ] 
		