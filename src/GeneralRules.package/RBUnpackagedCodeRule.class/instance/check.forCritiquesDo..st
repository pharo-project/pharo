running
check: anEntity forCritiquesDo: aCriticBlock

	anEntity package ifNotNil: [ :package |
		package isDefault ifFalse: [ ^ self ] ].
	
	aCriticBlock cull: (
		self critiqueFor: anEntity)