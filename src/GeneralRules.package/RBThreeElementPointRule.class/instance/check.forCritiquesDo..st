running
check: aNode forCritiquesDo: aCriticBlock
	| parentPoint |
	
	aNode isMessage ifFalse: [ ^ self ].
	aNode selector = #@ ifFalse: [ ^ self ].
	
	parentPoint := self
		findSuspiciousParentFor: aNode
		ifNone: [ ^self ].

	aCriticBlock cull: (
		self critiqueFor: parentPoint)		