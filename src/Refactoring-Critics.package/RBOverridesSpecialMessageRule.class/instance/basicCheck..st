running
basicCheck: aClass 
	| selectors |
	selectors := aClass isMeta 
		ifTrue: [ self metaclassShouldNotOverride ]
		ifFalse: [ self classShouldNotOverride ].
	^ selectors anySatisfy: [ :each |
		(aClass superclass notNil
			and: [ (aClass superclass canUnderstand: each)
			and: [ (aClass includesSelector: each) ] ]) ]