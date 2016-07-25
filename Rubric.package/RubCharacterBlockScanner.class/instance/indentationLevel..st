scanning
indentationLevel: anInteger
	super indentationLevel: anInteger.
	nextLeftMargin := leftMargin.
	indentationLevel timesRepeat: [
		nextLeftMargin := self nextTabXFrom: nextLeftMargin]