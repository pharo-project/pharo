event handling
mouseMove: event
	event isDraggingEvent ifFalse: [ ^ self ].
	event hand hasSubmorphs ifFalse: [ ^self ].
	(self wantsDroppedMorph: event hand submorphs first event: event) ifFalse: [ ^self ].
	
	(self container rowIndexContainingPoint: event position) 
		ifNotNil: [ :rowIndex | 
			self basicHighlightRowIndexes: { rowIndex }.
			self refresh ].
	
	(self container bounds containsPoint: event position) ifTrue: [ 
		(event position y <= (self container top + self autoScrollHeightLimit)) ifTrue: [ 
			^self verticalScrollBar scrollUp: 1
		].
		(event position y >= (self container bottom - self autoScrollHeightLimit)) ifTrue: [ 
			^self verticalScrollBar scrollDown: 1
		] 			
	]
