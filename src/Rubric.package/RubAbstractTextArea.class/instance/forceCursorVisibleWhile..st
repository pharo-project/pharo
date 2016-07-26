accessing cursor
forceCursorVisibleWhile: aBlock
	self cursor
		ifNotNil: [ :cur | 
			| prev |
			prev := cur stayVisible.
			cur stayVisible: true.
			aBlock
				ensure: [ cur stayVisible: prev ] ]