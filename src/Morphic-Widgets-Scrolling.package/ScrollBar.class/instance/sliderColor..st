access
sliderColor: aColor 
	"Change the color of the scrollbar to go with aColor."
	| buttonColor |
	super sliderColor: aColor.
	self lastPaneColor: aColor.
	buttonColor := self thumbColor.
	upButton color: buttonColor.
	downButton color: buttonColor.
	slider color: buttonColor slightlyLighter.	
	pagingArea
		fillStyle: self normalFillStyle;
		borderStyle: self normalBorderStyle.
	(self theme scrollbarPagingAreaCornerStyleIn: self window) = #rounded
		ifTrue: [self fillStyle: self normalButtonFillStyle]
		ifFalse: [self fillStyle: self normalFillStyle].
	self borderWidth: 0.
	^self adoptGradientColor: aColor