initialization
initialize
	super initialize.
	
	isChecked := false.
	isPressed := false.
	isSelected := false.
	
	self renderer: GLMInteractableBrickRenderer new.
	
	self disable.