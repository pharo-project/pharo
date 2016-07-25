accessing
pragmas
	^ pragmas ifNil: [
		pragmas := OrderedCollection new.
		SystemNavigation new allBehaviorsDo: [ :each |
			Pragma withPragmasIn: each do: [ :p |
				p keyword = self keyword ifTrue: [ 
					pragmas add: p ] ] ].
		pragmas ]