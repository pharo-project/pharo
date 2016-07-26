accessing
contentsOfArea: aRectangle into: aForm
	destForm 
		displayOn: aForm 
		at:  aRectangle origin
		clippingBox: (0@0 extent: aRectangle extent).
	^aForm