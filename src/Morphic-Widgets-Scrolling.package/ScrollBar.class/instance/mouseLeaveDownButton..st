mouse
mouseLeaveDownButton: event
	"The mouse has left the down button."

	event redButtonPressed
		ifFalse: [ 
			downButton
				fillStyle: self normalButtonFillStyle;
				borderStyle: self normalButtonBorderStyle;
				changed ]