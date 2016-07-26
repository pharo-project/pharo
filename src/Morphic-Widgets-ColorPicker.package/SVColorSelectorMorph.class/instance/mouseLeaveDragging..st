event handling
mouseLeaveDragging: evt
	"Make the location morph visible when leaving."
	
	evt hand showTemporaryCursor: nil.
	self showLocation