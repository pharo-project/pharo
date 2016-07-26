private
resetSelectors
	selectors := (SortedCollection new: 500) sortBlock: 
		[ :a :b | 
		| scoreA scoreB |
		scoreA := scoreB := 0.
		(a contents 
			beginsWithEmpty: narrowString
			caseSensitive: NECPreferences caseSensitive) ifFalse: [ scoreA := 2 ].
		(b contents 
			beginsWithEmpty: narrowString
			caseSensitive: NECPreferences caseSensitive) ifFalse: [ scoreB := 2 ].
		a contents < b contents
			ifTrue: [ scoreB := scoreB + 1 ]
			ifFalse: [ scoreA := scoreA + 1 ].
		scoreA < scoreB ]