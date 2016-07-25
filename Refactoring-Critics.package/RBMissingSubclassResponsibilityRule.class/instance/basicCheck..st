running
basicCheck: aClass 
	| subs |
	subs := aClass subclasses.
	(subs size > 1 and: [ aClass isMeta not ]) ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each selectors ].
		^ sels asSet anySatisfy: 
			[ :each |
			((sels occurrencesOf: each) == subs size and: [ (aClass canUnderstand: each) not ]) ] ].
	
		^ false.