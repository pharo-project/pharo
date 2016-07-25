mouse
mouseLeaveUpButton: event
	"The mouse has left the up button."

	event redButtonPressed
		ifFalse: [ 
			upButton
				fillStyle: self normalButtonFillStyle;
				borderStyle: self normalButtonBorderStyle;
				changed ]