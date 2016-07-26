floating
layoutFloatingLeft: aBrick in: newBounds
	| left top |
	
	left := aBrick marginLeft.
	top := aBrick marginTop.
	
	aBrick brickBounds: (left@top extent: aBrick extent).
	
	^ 0