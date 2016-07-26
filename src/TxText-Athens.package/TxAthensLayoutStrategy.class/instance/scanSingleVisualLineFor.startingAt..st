scanning
scanSingleVisualLineFor: aLayout startingAt: aPosition
	"Scan a single visual line until meeting stop condition.
	Note, the incoming position can be in the middle of text line (because of wrapping)"
	
	layout := aLayout.
	currentPosition := aPosition.	
	self startNewLine.
	metStopCondition := false.
	
	[ currentPosition := currentPosition span accept: self. metStopCondition ] whileFalse.

	^ currentLine