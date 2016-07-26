mouse
mouseLeavePagingArea: event
	"The mouse has left the paging area."
	
	pagingArea
		fillStyle: self normalFillStyle;
		borderStyle: self normalBorderStyle;
		changed.
	slider
		fillStyle: self normalThumbFillStyle;
		borderStyle: self normalThumbBorderStyle;
		changed.
	upButton
		fillStyle: self normalButtonFillStyle;
		borderStyle: self normalButtonBorderStyle;
		changed.
	downButton
		fillStyle: self normalButtonFillStyle;
		borderStyle: self normalButtonBorderStyle;
		changed