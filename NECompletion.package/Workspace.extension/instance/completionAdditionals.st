*necompletion
completionAdditionals
	| additionals |
	additionals := SortedCollection new.
	bindings ifNotNil: 
		[ bindings keysDo: 
			[ :each | 
			additionals add: (NECLocalEntry 
					contents: each
					type: #local) ] ].
	^ additionals