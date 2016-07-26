floating
layoutFloatingRight: aBrick in: newBounds
	| left top |
	
	left := aBrick owner width - aBrick width - aBrick marginRight - aBrick owner paddingRight.
	top := aBrick marginTop + aBrick owner paddingTop.
	
	aBrick brickBounds: (left@top extent: aBrick extent).
	
	^ 0