running
checkClass: aClass 
	| selectors |
	selectors := aClass isMeta 
		ifTrue: [ self metaclassShouldNotOverride ]
		ifFalse: [ self classShouldNotOverride ].
	selectors do: [ :each |
		(aClass superclass notNil
			and: [ (aClass superclass canUnderstand: each)
			and: [ (aClass includesSelector: each) ] ])
				ifTrue: [ result addClass: aClass selector: each ] ]