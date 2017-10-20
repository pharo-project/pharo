running
check: aNode forCritiquesDo: aCriticBlock

	| receiver |

	aNode isMessage ifFalse: [ ^ self ].
	receiver := aNode receiver.

	receiver isVariable ifFalse: [ ^ self ].
	receiver binding ifNil: [ receiver doSemanticAnalysis ].
	receiver isGlobal ifFalse: [ ^ self ]. 
		
	(self
		check: aNode selector
		forDeprecationIn: receiver name ) ifFalse: [ ^ self ]. 
				
	aCriticBlock cull: (self critiqueFor: aNode) 