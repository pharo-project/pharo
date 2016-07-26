mouse
mouseLeaveThumb: event
	"The mouse has left the thumb."

	| buttonFillStyle buttonBorderStyle |
	event redButtonPressed ifFalse: [ 
		slider
			fillStyle: self normalThumbFillStyle;
			borderStyle: self normalThumbBorderStyle;
			changed ].
	
	"Update buttons"
	(self containsPoint: event position)
		ifTrue: [ 
			buttonFillStyle := self mouseOverPagingAreaButtonFillStyle.
			buttonBorderStyle := self mouseOverPagingAreaButtonBorderStyle ]
		ifFalse: [ 
			buttonFillStyle := self normalButtonFillStyle.
			buttonBorderStyle := self normalButtonBorderStyle ].	
	upButton
		fillStyle: buttonFillStyle;
		borderStyle: buttonBorderStyle.
	downButton
		fillStyle: buttonFillStyle;
		borderStyle: buttonBorderStyle.