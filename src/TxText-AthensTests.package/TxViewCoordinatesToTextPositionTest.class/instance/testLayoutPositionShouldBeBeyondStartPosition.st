tests
testLayoutPositionShouldBeBeyondStartPosition
	|  oneCharRight layoutPosition |
	
	oneCharRight := view layout startPosition copy moveRight.
	layoutPosition := view layoutPositionAt: oneCharRight ifOutside: [  self assert: false ].

	self assert: view text == oneCharRight text.
	
	self assert: layoutPosition baselinePoint x > 0.