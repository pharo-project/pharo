accessing
rowIndexContainingPoint: aPoint
	self exposedRows keysAndValuesDo: [ :rowIndex :row | 
		(row bounds containsPoint: aPoint)
			ifTrue: [ ^ rowIndex ] ].
	^ nil