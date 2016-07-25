conversion
moveTo: aPoint 
	" create a new contour "
	
	self newContour.
	currentContour add: aPoint.