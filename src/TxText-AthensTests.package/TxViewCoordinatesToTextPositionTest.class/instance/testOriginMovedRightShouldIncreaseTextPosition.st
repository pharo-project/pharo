tests
testOriginMovedRightShouldIncreaseTextPosition
	| position oneCharRight pt layoutPosition |
	
	oneCharRight := view layout startPosition moveRight.
	layoutPosition := view layoutPositionAt: oneCharRight ifOutside: [  self assert: false ].
	pt := layoutPosition baselinePointInView.
	
	position := view textPositionAt: pt + (1@0).

	self assert: position = oneCharRight