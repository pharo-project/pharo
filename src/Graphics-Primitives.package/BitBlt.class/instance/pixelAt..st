copying
pixelAt: aPoint 
	"Assumes this BitBlt has been set up specially (see the init message,
	BitBlt bitPeekerFromForm:.  Returns the pixel at aPoint."
	sourceX := aPoint x.
	sourceY := aPoint y.
	destForm unhibernate.	"before poking"
	destForm bits 
		at: 1
		put: 0.	"Just to be sure"
	self copyBits.
	^ destForm bits at: 1