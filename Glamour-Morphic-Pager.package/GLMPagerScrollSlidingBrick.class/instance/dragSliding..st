slider-draging
dragSliding: evt
	| newLeft value|
	
	newLeft := (evt position x - self owner globalBounds left - startDragX).
	value := self computeScrollValue: newLeft.
	
	self scrollTo: value.
	self synchronizeScrollTo: value.