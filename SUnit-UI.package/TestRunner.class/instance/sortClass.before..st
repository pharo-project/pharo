utilities
sortClass: aFirstClass before: aSecondClass
	| first second |
	first := aFirstClass withAllSuperclasses reversed.
	second := aSecondClass withAllSuperclasses reversed.
	1 to: (first size min: second size) do: [ :index | 
		(first at: index) == (second at: index)
			ifFalse: [ ^ (first at: index) name <= (second at: index) name ] ].
	^ second includes: aFirstClass.