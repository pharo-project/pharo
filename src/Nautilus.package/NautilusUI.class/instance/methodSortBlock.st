private
methodSortBlock
	^ [ :a :b |
		"If one is binary and the other isn't, sort the binary first.
		Otherwise, sort them by Character values."
		a selector isBinary = b selector isBinary
			ifTrue: [ a selector <= b selector ]
			ifFalse: [ a selector isBinary ] ]