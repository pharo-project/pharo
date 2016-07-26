update
updateSliderBounds
	"Initialize the receiver's slider."
	
	self 
		updateUpButtonBounds;
		updateDownButtonBounds;
		updatePagingAreaBounds.
	super updateSliderBounds.
	