event handling
eventPositionInScene: evt
	| pt | 
	
	pt := evt position - self topLeft.
	
	^ transform transform: pt