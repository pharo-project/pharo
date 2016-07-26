drawing
forLinesIn: aVisibleRect do: aBlock
	(self lineIndexForPoint: aVisibleRect topLeft) to: 
	(self lineIndexForPoint: aVisibleRect bottomRight) do: 
		[:i | aBlock value: (self lines at: i) ]