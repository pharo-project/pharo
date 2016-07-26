slider-draging
dragResizingRight: anEvent
	| left right |

	left := self brickBounds left.
	right := anEvent position x - self owner globalBounds left - (startDragX - self computeWidth).
	right := (right max: (left + self pagerModel circleButtonSize)) min: self owner width.
	
	self setPosition: left@right
	