actions
pointSizeString: aText
	
	| string number |
	string := aText asString trimBoth.
	string isEmpty ifTrue: [^self].
	string
		detect: [:c | c isDigit not and: [c ~= $.]]
		ifFound: [^self].
	[number := string asNumber asFloat] on: Error do: [:e | ^self].
	(number < 1 or: [number > 1024])
		ifTrue: [^self].

	pointSizeMorph ifNotNil: [pointSizeMorph hasUnacceptedEdits: false].
	model pointSize: number