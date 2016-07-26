*Athens-Morphic
forLinesIn: aVisibleRect do: aBlock
	(self lineIndexForPoint: aVisibleRect topLeft) to: 
	(self lineIndexForPoint: aVisibleRect bottomRight) do: 
		[:i | aBlock value: (lines at: i) ]