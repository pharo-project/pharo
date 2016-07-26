mouse events
mouseLeave: evt
	"Reinstate the old border style."

	oldBorderStyle ifNil: [^self borderNormal].
	self borderStyle: oldBorderStyle. 
	oldBorderStyle := nil.
		