drawing
strokeRectangle: rect width: aWidth

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin 
			to: rect corner
			fillIndex: 0
			borderWidth: aWidth asInteger
			borderColor: currentFill.

	self copyBits.
	