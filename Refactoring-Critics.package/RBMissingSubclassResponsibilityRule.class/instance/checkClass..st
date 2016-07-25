running
checkClass: aClass 
	| subs |
	subs := aClass subclasses.
	(subs size > 1 and: [ aClass isMeta not ]) ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		sels asSet do: 
			[ :each | 
			((sels occurrencesOf: each) == subs size and: [ (aClass canUnderstand: each) not ]) ifTrue: 
				[ | envName |
				envName := aClass name , '>>' , each.
				subs do: 
					[ :subClass | 
					result 
						addClass: subClass
						selector: each
						into: envName ] ] ] ]