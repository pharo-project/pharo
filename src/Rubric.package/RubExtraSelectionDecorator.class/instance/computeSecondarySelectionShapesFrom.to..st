drawing
computeSecondarySelectionShapesFrom: topLeft to: bottomRight
	| primary secondarySelRanges otherRanges topLeftBlk |
	primary := self primarySelectionInterval.
	topLeftBlk := self characterBlockAtPoint: topLeft.
	otherRanges := findReplaceRanges ifNil: [ #() ].
	self selection asString trimBoth ifEmpty: [ ^secondarySelectionShapes := #() ].
	(secondarySelRanges := (((self visibleStringFrom: topLeft to: bottomRight) allRangesOfSubstring: self selection asString)
		collect: [ :r | r + topLeftBlk stringIndex - 1 ])
		reject: [ :i | 
			(otherRanges anySatisfy: [ :r | (r rangeIncludes: i first) or: [ r rangeIncludes: i last ] ])
				or: [ i = primary or: [ i first > self text size ] ] ]) collect: [ :i | i first to: (i last min: self text size) ].
	secondarySelRanges
		do: [ :r | self text addAttribute: RubTextSelectionColor secondarySelection from: r first to: r last ].
	^ secondarySelectionShapes := secondarySelRanges
		collect: [ :r | 
			RubSurfaceSelectionShape
				vertices: (self verticesFrom: r first to: r last + 1)
				firstIndex: r first
				lastIndex: r last + 1
				color: (RubTextSelectionColor secondarySelection backgroundColor alpha: 0.6)
				borderWidth: 0
				borderColor: RubTextSelectionColor secondarySelection backgroundColor muchDarker]