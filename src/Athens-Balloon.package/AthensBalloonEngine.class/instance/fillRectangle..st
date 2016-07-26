drawing
fillRectangle: rect 

	currentFill = 0 ifTrue: [ ^ self ]. "no fill "
	self primAddRectFrom: rect origin 
			to: rect corner
			fillIndex: currentFill
			borderWidth: 0
			borderColor: 0.

	self copyBits.
	