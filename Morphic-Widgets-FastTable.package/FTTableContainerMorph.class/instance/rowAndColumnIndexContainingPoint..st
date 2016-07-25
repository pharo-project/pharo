accessing
rowAndColumnIndexContainingPoint: aPoint
	"answer a tuple containing { rowIndex. columnNumber } to be used for menus, etc. 
	 (check senders for references)"

	self exposedRows keysAndValuesDo: [ :rowIndex :row | 
		(row bounds containsPoint: aPoint) ifTrue: [ 
			row submorphs withIndexDo: [ :each :columnIndex |
				 (each bounds containsPoint: aPoint) 
					ifTrue: [ ^ { rowIndex. columnIndex } ] ] ] ].
	^ {nil. nil}