drawing
strokeRectangle: rect borderWidth: width

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin 
			to: rect corner
			fillIndex: 0
			borderWidth: width
			borderColor: currentFill.

	self copyBits.
	