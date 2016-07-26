geometry
extent: aPoint
	|newTop patch offset|
	super extent: aPoint.
	
	offset := (self height - self textArea height) / 2.0.
	patch := offset fractionPart rounded - offset sign.
	newTop := (offset + patch) truncated abs.
	
	self textArea top: newTop.