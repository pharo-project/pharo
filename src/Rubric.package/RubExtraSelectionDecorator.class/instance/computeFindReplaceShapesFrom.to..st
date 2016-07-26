computation
computeFindReplaceShapesFrom: topLeft to: bottomRight
	| topLeftBlk |
	findReplaceShapes := nil.
	self findRegex ifNil: [ ^self ].
	topLeftBlk := self characterBlockAtPoint: topLeft.
	findReplaceRanges := ((self findRegex matchingRangesIn: (self visibleStringFrom: topLeft to: bottomRight))
		collect: [ :r | r + topLeftBlk stringIndex - 1 ]).
	findReplaceRanges do: [ :r | self text addAttribute: RubTextSelectionColor findReplaceSelection from: r first to: r last ].
	findReplaceShapes := findReplaceRanges
		collect: [ :r | 
			RubSurfaceSelectionShape
				vertices: (self verticesFrom: r first to: r last + 1)
				firstIndex: r first
				lastIndex: r last + 1
				color: (RubTextSelectionColor findReplaceSelection backgroundColor alpha: 0.1)
				borderWidth: 2
				borderColor: RubTextSelectionColor findReplaceSelection backgroundColor muchDarker].
	^ findReplaceShapes