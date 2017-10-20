running
check: aMethod forCritiquesDo: aCriticBlock
	(aMethod overriddenMethods anySatisfy: [ :method |
		method hasPragmaNamed: #shouldReturn. ])
			ifFalse: [ ^ self ].
	
	aMethod ast lastIsReturn ifTrue: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aMethod)