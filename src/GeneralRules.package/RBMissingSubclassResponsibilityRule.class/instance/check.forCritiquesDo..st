enumerating
check: aClass forCritiquesDo: aCritiqueBlock
	
	| subs |
	subs := aClass subclasses.
	(subs size > 1 and: [ aClass isMeta not ]) ifTrue: 
		[ | sels noSuperSels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		noSuperSels := sels asSet select: [ :each | 
			((sels occurrencesOf: each) == subs size and: [
			(aClass canUnderstand: each) not ]) ].
		noSuperSels ifNotEmpty: [ 
			noSuperSels collect: [ :selector |
				(ReMissingMethodCritique
					for: aClass
					by: self
					class: aClass
					selector: selector)
					beSubclassResponsibility;
					yourself ]
				thenDo: [ :critic |
					aCritiqueBlock cull: critic ].
				^ self ] ].
		