conversion
newContour
	contours ifNil: [ contours := OrderedCollection new ].
	
	currentContour := contours add: OrderedCollection new.