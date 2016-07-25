mouse
mouseEnterDownButton: event
	"The mouse has entered the down button."
	
	downButton
		fillStyle: self mouseOverButtonFillStyle;
		borderStyle: self mouseOverButtonBorderStyle;
		changed