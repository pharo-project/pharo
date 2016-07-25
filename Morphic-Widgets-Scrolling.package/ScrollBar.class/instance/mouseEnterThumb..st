mouse
mouseEnterThumb: event
	"The mouse has entered the thumb."
	
	slider
		fillStyle: self mouseOverThumbFillStyle;
		borderStyle: self mouseOverThumbBorderStyle;
		changed.
	upButton
		fillStyle: self mouseOverThumbButtonFillStyle;
		borderStyle: self mouseOverThumbButtonBorderStyle;
		changed.
	downButton
		fillStyle: self mouseOverThumbButtonFillStyle;
		borderStyle: self mouseOverThumbButtonBorderStyle;
		changed