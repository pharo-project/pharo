running
checkClass: aClass 
	| subs |
	subs := aClass subclasses.
	subs size > 1 ifTrue: 
		[ | sels |
		sels := Bag new.
		subs do: [ :each | sels addAll: each instVarNames ].
		sels asSet do: 
			[ :val | 
			| count |
			count := sels occurrencesOf: val.
			count == subs size ifTrue: 
				[ result 
					addClass: aClass
					instanceVariable: val ] ] ]