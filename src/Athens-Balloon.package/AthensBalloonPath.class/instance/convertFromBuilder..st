converting
convertFromBuilder: aBuilder
	| segment cContours |
	bounds := aBuilder pathBounds.
	segment := aBuilder pathStart.
	

	[ segment notNil ] whileTrue: [
		segment visitWith: self.
		segment := segment next ].
	
	" finally make all OrderedColllections ==> Array "
	cContours := Array new: contours size.
	1 to: cContours size do: [:i |
		cContours at: i put: (contours at: i) asArray.
		].
	contours := cContours.