geometry
extent: aPoint
	|left top right bottom|
	
	super extent: aPoint.
	
	left := self brickRoot marginLeft.
	top := self brickRoot marginTop.
	
	right := self extent x - self brickRoot marginRight.
	bottom := self extent y - self brickRoot marginBottom.
	
	self isHFilling ifTrue: [ self brickRoot width: right - left ].
	self isVFilling ifTrue: [ self brickRoot height: bottom - top ].
	self brickRoot wrappedBounds bounds: (left@top corner: (right@bottom)).
	
	self brickRoot doLayoutForce