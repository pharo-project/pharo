mouse
mouseUpInSlider: event
	"The mouse button has been released."
	
	sliderShadow hide.
	(slider containsPoint: event position)
		ifTrue: [slider
					fillStyle: self mouseOverThumbFillStyle;
					borderStyle: self mouseOverThumbBorderStyle]
		ifFalse: [self mouseLeaveThumb: event].
	slider changed