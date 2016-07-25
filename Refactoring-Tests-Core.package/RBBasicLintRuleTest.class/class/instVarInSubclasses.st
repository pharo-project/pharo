miscellaneous
instVarInSubclasses
	| detector |
	detector := self new.
	detector name: 'Instance variables defined in all subclasses'.
	detector result: nil pullUpInstVar.
	detector classBlock: 
			[:context :result | 
			| subs |
			subs := context selectedClass subclasses.
			subs size > 1 ifTrue: 
					[| sels |
					sels := Bag new.
					subs do: [:each | sels addAll: each instVarNames].
					sels asSet do: 
							[:val | 
							(sels occurrencesOf: val) == subs size
								ifTrue: [result addInstVar: val for: context selectedClass]]]].
	^detector