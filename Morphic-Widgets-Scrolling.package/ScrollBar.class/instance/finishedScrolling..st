access
finishedScrolling: event
	"Scrolling has finished for a button."
	
	self finishedScrolling.
	(self containsPoint: event position)
		ifTrue: [pagingArea
				fillStyle: self mouseOverFillStyle;
				borderStyle: self mouseOverBorderStyle]
		ifFalse: [pagingArea
				fillStyle: self normalFillStyle;
				borderStyle: self normalBorderStyle].
	(upButton containsPoint: event position)
		ifTrue: [upButton
					fillStyle: self mouseOverButtonFillStyle;
					borderStyle: self mouseOverButtonBorderStyle]
		ifFalse: [(self containsPoint: event position)
					ifTrue: [upButton
							fillStyle: self mouseOverPagingAreaButtonFillStyle;
							borderStyle: self mouseOverPagingAreaButtonBorderStyle]
					ifFalse: [upButton
							fillStyle: self normalButtonFillStyle;
							borderStyle: self normalButtonBorderStyle]].
	(downButton containsPoint: event position)
		ifTrue: [downButton
					fillStyle: self mouseOverButtonFillStyle;
					borderStyle: self mouseOverButtonBorderStyle]
		ifFalse: [(self containsPoint: event position)
					ifTrue: [downButton
							fillStyle: self mouseOverPagingAreaButtonFillStyle;
							borderStyle: self mouseOverPagingAreaButtonBorderStyle]
					ifFalse: [downButton
							fillStyle: self normalButtonFillStyle;
							borderStyle: self normalButtonBorderStyle]]