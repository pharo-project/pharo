mouse
mouseEnterUpButton: event
	"The mouse has entered the up button."
	
	upButton
		fillStyle: self mouseOverButtonFillStyle;
		borderStyle: self mouseOverButtonBorderStyle;
		changed