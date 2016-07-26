event handling
mouseEnterDragging: evt
	"Make the location morph invisible when entering."
	
	self hideLocation.
	evt hand showTemporaryCursor: (Cursor crossHair copy offset: -9 @ -9).