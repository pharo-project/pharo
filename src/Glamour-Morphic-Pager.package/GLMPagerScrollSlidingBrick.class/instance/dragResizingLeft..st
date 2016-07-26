slider-draging
dragResizingLeft: anEvent
	| left right |
	
	right := self brickBounds right.
	left := anEvent position x - self owner globalBounds left - startDragX.
	left := (left max: 0) min: (right - self pagerModel circleButtonSize).
	
	self setPosition: left@right
	