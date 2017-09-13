running
check: aNode forCritiquesDo: aCritiqueBlock


	aNode isMessage ifFalse: [ ^ self ].
	(#(#select: #collect: #reject:) includes: aNode selector) ifFalse: [ ^ self ].
	aNode isUsed ifTrue: [ ^ self ].
	
	aCritiqueBlock cull: ( self critiqueFor: aNode )