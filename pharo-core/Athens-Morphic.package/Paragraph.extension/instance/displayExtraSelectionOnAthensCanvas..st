*Athens-Morphic
displayExtraSelectionOnAthensCanvas: anAthensCanvas
	| visibleRectangle |
	visibleRectangle := anAthensCanvas clipRect.
	refreshExtraSelection = true
		ifTrue: [ self buildSelectionBlocksFrom: visibleRectangle topLeft to: visibleRectangle bottomRight.
			refreshExtraSelection := false ].
		
	extraSelectionBlocks
		ifNotNil: [ 
			self forLinesIn: visibleRectangle	do: [ :line | 
					extraSelectionBlocks
						do: [ :selblock | self displaySelectionBlock: selblock inLine: line onAthensCanvas: anAthensCanvas ] ] ]