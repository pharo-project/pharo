running
check: aClass forCritiquesDo: aCriticBlock
	(aClass isMeta or: [ aClass isTestCase ])
		ifTrue: [ ^ false ].
	(aClass hasComment not) ifTrue: [ 
		aCriticBlock cull: (self critiqueFor: aClass) ]